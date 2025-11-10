-- Runs the list of instructions
-- The function "interpret" is responsible for the main interpreter loop
-- The function "interpretProgram" just sets up the initial interpreter state and calls "interpret"
-- The function "interpretStep" executes a single instruction and updates the interpreter state

module Interpreter where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy (unpack)
import Debug.Trace (traceM)
import Flatten hiding (Jump)
import Flatten qualified
import GHC.Generics (Generic)
import Language
import Text.Pretty.Simple (pShow)

data SingleValue
  = NumericValue Double
  | BoolValue Bool
  | Undefined
  deriving (Show, Eq, Generic)

instance ToJSON SingleValue

instance FromJSON SingleValue

type Environment = Map String Value

data FinalisedVariableAccess = FinalisedDirectAccess String | FinalisedSubscript String Int

data Value = Single SingleValue | Array [SingleValue]
  deriving (Show, Eq, Generic)

instance ToJSON Value

instance FromJSON Value

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

floatRepresentsInteger :: Double -> Bool
floatRepresentsInteger n = n == fromIntegral (truncate n)

finaliseVariableAccess :: Environment -> VariableAccess -> Result FinalisedVariableAccess
finaliseVariableAccess env var = case var of
  DirectVariableAccess v -> Right $ FinalisedDirectAccess v
  VariableSubscript v indexExpr -> do
    indexResult <- evaluateExpression env indexExpr
    case indexResult of
      Single (NumericValue index) ->
        if floatRepresentsInteger index
          then Right $ FinalisedSubscript v (truncate index)
          else Left "Array index must be an integer"
      _ -> Left "Invalid array subscript"

evaluateBinaryOp :: Environment -> BinaryOp -> Result Value
evaluateBinaryOp env (BinaryOp left op right) = do
  leftVal <- evaluateExpression env left
  rightVal <- evaluateExpression env right
  case (leftVal, rightVal) of
    (leftVal, rightVal) | leftVal == Single Undefined || rightVal == Single Undefined -> Left "Undefined value in binary operation"
    (Array l, Single (NumericValue r)) -> case op of
      Subscript ->
        if floatRepresentsInteger r
          then
            Right (Array (take (floor r) l))
          else Left "Array subscript must be an integer"
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

evaluateBuildExpression :: Environment -> Expression -> Result Value
evaluateBuildExpression env lengthExpr = do
  lengthVal <- evaluateExpression env lengthExpr

  case lengthVal of
    Single (NumericValue n) ->
      if floatRepresentsInteger n
        then Right (Array (replicate (floor n) Undefined))
        else Left "Invalid length for build expression. Length must be an integer."
    _ -> Left "Invalid length for build expression. Length must be a number."

evaluateExpression :: Environment -> Expression -> Result Value
evaluateExpression env expr = case expr of
  LiteralExpression lit -> Right $ evaluateLiteral env lit
  VariableExpression var -> do
    finalVar <- finaliseVariableAccess env var
    getVariable env finalVar
  BinaryExpression binaryOp -> evaluateBinaryOp env binaryOp
  UnaryExpression unaryOp -> evaluateUnaryOp env unaryOp
  BuildExpression lengthExpr -> evaluateBuildExpression env lengthExpr

setVariable :: Environment -> FinalisedVariableAccess -> Value -> Result Environment
setVariable env var value = case var of
  FinalisedDirectAccess v -> Right $ Map.insert v value env
  FinalisedSubscript v index -> case value of
    Single val -> case Map.findWithDefault (Single Undefined) v env of
      Array arr ->
        if index < List.length arr
          then Right $ Map.insert v (Array (List.take index arr ++ [val] ++ List.drop (index + 1) arr)) env
          else Left $ fromString ("Array index " ++ show index ++ " is out of bounds, array " ++ v ++ ", " ++ show arr ++ " has length " ++ show (List.length arr))
      _ -> Left "Invalid array subscript"
    _ -> Left "Arrays can't contain arrays (yet)."

getVariable :: Environment -> FinalisedVariableAccess -> Result Value
getVariable env var = case var of
  FinalisedDirectAccess v -> Right $ Map.findWithDefault (Single Undefined) v env
  FinalisedSubscript v index -> do
    case Map.findWithDefault (Single Undefined) v env of
      Array arr ->
        if index < length arr
          then Right . Single $ arr !! index
          else Left "Array index out of bounds"
      _ -> Left "Invalid array subscript"

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

swapVariables :: Environment -> VariableAccess -> VariableAccess -> Result Environment
swapVariables env var1 var2 = do
  var1final <- finaliseVariableAccess env var1
  var2final <- finaliseVariableAccess env var2
  val1 <- getVariable env var1final
  val2 <- getVariable env var2final
  setVariable env var1final val2
  setVariable env var2final val1

interpretStep :: Environment -> Instruction -> (Environment, Result ControlFlow)
interpretStep env instr = do
  -- traceM ("Interpreting instruction: " ++ unpack (pShow instr))
  case instr of
    Flatten.Jump n condition -> case evaluateJumpInstruction env n condition of
      Right ctrlFlow -> (env, Right ctrlFlow)
      Left err -> (env, Left err)
    Flatten.SwapVariables var1 var2 -> case swapVariables env var1 var2 of
      Right newEnv -> (newEnv, Right Continue)
      Left err -> (env, Left err)
    Flatten.AssignVariable var value -> case do
      finalAccess <- finaliseVariableAccess env var
      finalValue <- evaluateExpression env value
      setVariable env finalAccess finalValue of
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