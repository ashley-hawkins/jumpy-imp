-- Everything needed to represent the AST basically.
module Language where

data BinaryOperator = Addition | Subtraction | Multiplication | Division | Lt | Gt | Lte | Gte | Eq | Neq | And | Or
  deriving (Show, Eq)

data UnaryOperator = UnaryNegate | UnaryPlus | UnaryNot
  deriving (Show, Eq)

data StatementUniquePart
  = IfStatement
      { condition :: Expression,
        thenBranch :: StatementList,
        elseBranch :: Maybe StatementList
      }
  | GoToStatement
      { goToLine :: Int
      }
  | AssignmentStatement
      { variable :: VariableAccess,
        value :: Expression
      }
  | SwapStatement
      { var1 :: VariableAccess,
        var2 :: VariableAccess
      }
  | ReturnStatement
      { returnValue :: Expression
      }
  deriving (Show, Eq)

data Statement = Statement
  { uniquePart :: StatementUniquePart,
    lineNumber :: Int
  }
  deriving (Show, Eq)

type StatementList = [Statement]

data BinaryOp = BinaryOp
  { left :: Expression,
    binaryOperator :: BinaryOperator,
    right :: Expression
  }
  deriving (Show, Eq)

data UnaryOp = UnaryOp
  { unaryOperator :: UnaryOperator,
    operand :: Expression
  }
  deriving (Show, Eq)

data Literal = LiteralFloat Double | LiteralBool Bool
  deriving (Show, Eq)

newtype VariableAccess = VariableAccess String
  deriving (Show, Eq)

data Expression
  = BinaryExpression BinaryOp
  | UnaryExpression UnaryOp
  | LiteralExpression Literal
  | VariableExpression VariableAccess
  deriving (Show, Eq)