module Interpreter where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy (unpack)
import Debug.Trace (traceM)
import Flatten hiding (Jump)
import Flatten qualified
import Language
import Text.Pretty.Simple (pShow)

data SingleValue
  = NumericValue Double
  | BoolValue Bool
  deriving (Show, Eq)

type Environment = Map String Value

data Value = Single SingleValue | Array [SingleValue] | Undefined
  deriving (Show, Eq)

data InterpreterState = InterpreterState
  { environment :: Environment,
    programCounter :: Int,
    program :: [Instruction]
  }
  deriving (Show)

type Error = Text

data ControlFlow
  = Continue
  | Jump Int
  | Halt Value

type Result a = Either Error a

stepState :: InterpreterState -> Environment -> ControlFlow -> InterpreterState
stepState state newEnv controlFlow = case controlFlow of
  Continue -> state {environment = newEnv, programCounter = programCounter state + 1}
  Jump n -> state {environment = newEnv, programCounter = n}
  Halt val -> state {environment = Map.insert "$return" val newEnv, programCounter = -1, program = []}

interpretCurrentInstruction :: InterpreterState -> (Environment, Result ControlFlow)
interpretCurrentInstruction InterpreterState {environment = env, programCounter = pc, program = prog} =
  let currentInstruction = prog !! pc
   in interpretStep env currentInstruction

interpret :: InterpreterState -> Result InterpreterState
interpret state | not $ pcIsValid (programCounter state) = Right state
  where
    pcIsValid pc = pc >= 0 && pc < length (program state)
interpret state =
  let (newEnv, controlFlowOrError) = interpretCurrentInstruction state
   in do
        controlFlow <- controlFlowOrError
        interpret $ stepState state newEnv controlFlow

evaluateLiteral :: Environment -> Literal -> Value
evaluateLiteral env lit = case lit of
  LiteralFloat n -> Single (NumericValue n)
  LiteralBool b -> Single (BoolValue b)

evaluateBinaryOp :: Environment -> BinaryOp -> Result Value
evaluateBinaryOp env (BinaryOp left op right) = do
  leftVal <- evaluateExpression env left
  rightVal <- evaluateExpression env right
  case (leftVal, rightVal) of
    (leftVal, rightVal) | leftVal == Undefined || rightVal == Undefined -> Left "Undefined value in binary operation"
    (Single (NumericValue l), Single (NumericValue r)) -> case op of
      Addition -> Right (Single (NumericValue (l + r)))
      Subtraction -> Right (Single (NumericValue (l - r)))
      Multiplication -> Right (Single (NumericValue (l * r)))
      Division -> Right (Single (NumericValue (l / r)))
      Lt -> Right (Single (BoolValue (l < r)))
      Gt -> Right (Single (BoolValue (l > r)))
      Lte -> Right (Single (BoolValue (l <= r)))
      Gte -> Right (Single (BoolValue (l >= r)))
      Eq -> Right (Single (BoolValue (l == r)))
      Neq -> Right (Single (BoolValue (l /= r)))
      _ -> Left "Type mismatch in binary operation"
    (Single (BoolValue b1), Single (BoolValue b2)) -> case op of
      And -> Right (Single (BoolValue (b1 && b2)))
      Or -> Right (Single (BoolValue (b1 || b2)))
      _ -> Left "Type mismatch in binary operation"
    _ -> Left "Type mismatch in binary operation"

evaluateUnaryOp :: Environment -> UnaryOp -> Result Value
evaluateUnaryOp env (UnaryOp op operand) = do
  operandVal <- evaluateExpression env operand
  case (op, operandVal) of
    (UnaryNegate, Single (NumericValue n)) -> Right (Single (NumericValue (-n)))
    (UnaryPlus, Single (NumericValue n)) -> Right (Single (NumericValue n))
    (UnaryNot, Single (BoolValue b)) -> Right (Single (BoolValue (not b)))
    _ -> Left "Type mismatch in unary operation"

evaluateExpression :: Environment -> Expression -> Result Value
evaluateExpression env expr = case expr of
  LiteralExpression lit -> Right $ evaluateLiteral env lit
  VariableExpression (VariableAccess var) -> Right $ Map.findWithDefault Undefined var env
  BinaryExpression binaryOp -> evaluateBinaryOp env binaryOp
  UnaryExpression unaryOp -> evaluateUnaryOp env unaryOp

swapVariables :: Environment -> String -> String -> Result Environment
swapVariables env var1 var2 = case (Map.lookup var1 env, Map.lookup var2 env) of
  (Just (Single v1), Just (Single v2)) -> Right $ Map.insert var1 (Single v2) (Map.insert var2 (Single v1) env)
  _ -> Left "Undefined variable in swap"

evaluateJumpInstruction :: Environment -> Int -> Maybe Expression -> Result ControlFlow
evaluateJumpInstruction env n condition = case condition of
  Just e -> conditionallyJump e
  Nothing -> Right (Jump n)
  where
    conditionallyJump :: Expression -> Result ControlFlow
    conditionallyJump e = case evaluateExpression env e of
      Right (Single (BoolValue True)) -> Right (Jump n)
      Right (Single (BoolValue False)) -> Right Continue
      Right _ -> Left "Invalid condition for jump: Not a boolean"
      Left err -> Left $ T.pack ("Invalid condition for jump: " ++ show err)

assignVariable :: Environment -> String -> Expression -> Result Environment
assignVariable env var value = do
  evaluatedValue <- evaluateExpression env value
  return $ Map.insert var evaluatedValue env

interpretStep :: Environment -> Instruction -> (Environment, Result ControlFlow)
interpretStep env instr = do
  -- traceM ("Interpreting instruction: " ++ unpack (pShow instr))
  case instr of
    Flatten.Jump n condition -> case evaluateJumpInstruction env n condition of
      Right ctrlFlow -> (env, Right ctrlFlow)
      Left err -> (env, Left err)
    Flatten.SwapVariables (VariableAccess var1) (VariableAccess var2) -> case swapVariables env var1 var2 of
      Right newEnv -> (newEnv, Right Continue)
      Left err -> (env, Left err)
    Flatten.AssignVariable (VariableAccess var) value -> case assignVariable env var value of
      Right newEnv -> (newEnv, Right Continue)
      Left err -> (env, Left err)
    Flatten.ReturnValue valueExpr -> case evaluateExpression env valueExpr of
      Right val -> (env, Right (Halt val))
      Left err -> (env, Left err)

interpretProgram :: [Instruction] -> Result InterpreterState
interpretProgram instructions =
  interpret
    InterpreterState
      { environment = Map.empty,
        programCounter = 0,
        program = instructions
      }

interpretProgramWithEnv :: [Instruction] -> Environment -> Result InterpreterState
interpretProgramWithEnv instructions env =
  interpret
    InterpreterState
      { environment = env,
        programCounter = 0,
        program = instructions
      }