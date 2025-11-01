{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Language (BinaryOp (BinaryOp), BinaryOperator (..), Expression (..), Literal (..), Statement (..), StatementList, StatementUniquePart (..), UnaryOp (..), VariableAccess (..), UnaryOperator (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Error = Void

type Input = Text

type Parser a = Parsec Error Input a

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pVariable :: Parser VariableAccess
pVariable = VariableAccess <$> lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

pInt :: Parser Literal
pInt = LiteralFloat <$> lexeme L.decimal <?> "integer literal"

pFloat :: Parser Literal
pFloat = LiteralFloat <$> lexeme L.float <?> "floating point literal"

pNumber :: Parser Literal
pNumber = choice [try pFloat, try pInt]

pBool :: Parser Literal
pBool =
  LiteralBool <$> (string "true" $> True <|> string "false" $> False) <?> "boolean literal"

pStatement :: Parser Statement
pStatement = do
  currentLineNumber <- unPos . sourceLine <$> getSourcePos
  uniquePart <- choice [try pGoToStatement, try pSwapStatement, try pAssignmentStatement, try pIfStatement]
  return $ Statement uniquePart currentLineNumber

pIfStatementHelper :: Parser a -> Maybe Pos -> Parser (a, StatementList)
pIfStatementHelper pHeader expectedIndentation = L.indentBlock scn p
  where
    p = do
      _ <- case expectedIndentation of
        Just ind -> Just <$> L.indentGuard sc EQ ind
        Nothing -> pure Nothing
      headerValue <- pHeader
      return (L.IndentSome Nothing (return . (headerValue,)) pStatement)

pIfStatementFirstPart = pIfStatementHelper pIfStatementIntroduction Nothing

pIfStatementSecondPart :: Pos -> Parser StatementList
pIfStatementSecondPart expectedIndentation = snd <$> pIfStatementHelper (pKeyword "else") (Just expectedIndentation)

pSwapStatement :: Parser StatementUniquePart
pSwapStatement = do
  var1 <- pVariable
  _ <- symbol "<->"
  SwapStatement var1 <$> pVariable

pAssignmentStatement :: Parser StatementUniquePart
pAssignmentStatement = do
  var <- pVariable
  _ <- symbol "<-"
  AssignmentStatement var <$> expr

pGoToStatement :: Parser StatementUniquePart
pGoToStatement = do
  _ <- pKeyword "goto" >> sc >> pKeyword "line"
  line <- lexeme L.decimal
  return (GoToStatement line)

pIfStatementIntroduction :: Parser (Pos, Expression)
pIfStatementIntroduction = do
  expectedIndentation <- L.indentLevel
  (expectedIndentation,) <$> between (pKeyword "if") (pKeyword "then") expr

pIfStatement :: Parser StatementUniquePart
pIfStatement = do
  ((expectedIndentation, thenCond), thenBranch) <- pIfStatementFirstPart
  elseBranch <- optional (pIfStatementSecondPart expectedIndentation)
  return (IfStatement thenCond thenBranch elseBranch)

pLiteral :: Parser Literal
pLiteral = choice [try pNumber, try pBool]

pTopLevelStatementList :: Parser [Statement]
pTopLevelStatementList = L.nonIndented scn (some pStatement)

term :: Parser Expression
term =
  choice
    [ try (LiteralExpression <$> pLiteral),
      try (VariableExpression <$> pVariable),
      parens expr
    ]

expr :: Parser Expression
expr = makeExprParser term table <?> "expression"

table =
  [ [ binaryOp Addition "+",
      binaryOp Subtraction "-",
      binaryOp And "&&",
      binaryOp Or "||",
      unaryOp UnaryNot "!",
      unaryOp UnaryNegate "-",
      unaryOp UnaryPlus "+"
    ],
    [ binaryOp Multiplication "*",
      binaryOp Division "/"
    ],
    [ binaryOp Gt ">",
      binaryOp Lt "<",
      binaryOp Gte ">=",
      binaryOp Lte "<=",
      binaryOp Eq "=",
      binaryOp Neq "/="
    ]
  ]

binaryOp opValue opText = InfixL ((\l r -> BinaryExpression (BinaryOp l opValue r)) <$ L.symbol sc opText)

unaryOp opValue opText = Prefix ((UnaryExpression . UnaryOp opValue) <$ L.symbol sc opText)