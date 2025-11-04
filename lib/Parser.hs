-- Ugly!!!

-- I totally copied some of the things done in https://markkarpov.com/tutorial/megaparsec.html while misunderstanding a lot of it.
-- So this parser is kind of broken. Rewriting will be fun.........................

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language (BinaryOp (BinaryOp), BinaryOperator (..), Expression (..), Literal (..), Statement (..), StatementList, StatementUniquePart (..), UnaryOp (..), UnaryOperator (..), VariableAccess (..))
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
  uniquePart <- choice [try pReturnStatement, try pGoToStatement, try pSwapStatement, try pAssignmentStatement, try pIfStatement] <?> "statement"
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
  SwapStatement var1 <$> pVariable <* some eol

pAssignmentStatement :: Parser StatementUniquePart
pAssignmentStatement = do
  var <- pVariable
  _ <- symbol "<-"
  AssignmentStatement var <$> expr <* some eol

pGoToStatement :: Parser StatementUniquePart
pGoToStatement = do
  _ <- pKeyword "goto" >> sc >> pKeyword "line"
  line <- lexeme L.decimal
  return (GoToStatement line)

pReturnStatement :: Parser StatementUniquePart
pReturnStatement = do
  _ <- pKeyword "return"
  ReturnStatement <$> expr <* some eol

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
      unaryOp UnaryNegate "-",
      unaryOp UnaryPlus "+"
    ],
    [ binaryOp Multiplication "*",
      binaryOp Division "/"
    ],
    [ binaryOp Gte ">=",
      binaryOp Lte "<=",
      binaryOp Gt ">",
      binaryOp Lt "<",
      binaryOp Eq "=",
      binaryOp Neq "/="
    ],
    [ binaryOp Or "||"
    ],
    [ binaryOp And "&&"
    ],
    [ unaryOp UnaryNot "!"
    ]
  ]

pEnvArgument :: Parser (VariableAccess, Expression)
pEnvArgument = do
  var <- pVariable
  _ <- symbol ":"
  expression <- expr
  return (var, expression)

stringToEnvArgument :: String -> Either (ParseErrorBundle Text Void) (VariableAccess, Expression)
stringToEnvArgument str =
  let txt = fromString str
   in parse (pEnvArgument <* eof) "" txt

parseFileToProgram :: String -> Text -> Either (ParseErrorBundle Text Void) [Statement]
parseFileToProgram = parse (pTopLevelStatementList <* (eof <?> "Top level EOF"))

binaryOp opValue opText = InfixL ((\l r -> BinaryExpression (BinaryOp l opValue r)) <$ L.symbol sc opText)

unaryOp opValue opText = Prefix ((UnaryExpression . UnaryOp opValue) <$ L.symbol sc opText)