-- Transforms the AST into a flat sequence of instructions

-- Expressions don't need to be flattened because you can't "goto" into the middle
-- of an expression so the interpreter is able to just evaluate the expression tree recursively
module Flatten where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tree (flatten)
import Language (Expression (UnaryExpression), Statement (Statement), StatementUniquePart (AssignmentStatement, GoToStatement, IfStatement, SwapStatement, ReturnStatement), UnaryOp (UnaryOp), UnaryOperator (UnaryNot), VariableAccess)
import Language qualified

data Instruction
  = Jump Int (Maybe Expression)
  | SwapVariables VariableAccess VariableAccess
  | AssignVariable VariableAccess Expression
  | ReturnValue Expression
  deriving (Show)

unconditionalJump n = Jump n Nothing

conditionalJump n cond = Jump n $ Just cond

data IntermediateInstruction
  = Final Instruction
  | -- Special case because jumping by line needs to be transformed into a
    -- regular Jump after lines have been mapped to instruction positions
    JumpByLine Int (Maybe Expression)
  deriving (Show)

-- Get the number of instructions it would take to represent a statement (including nested statements)
getInstructionCount :: Statement -> Int
getInstructionCount (Statement uniquePart lineNumber) = case uniquePart of
  GoToStatement _ -> 1
  SwapStatement _ _ -> 1
  AssignmentStatement _ _ -> 1
  ReturnStatement _ -> 1
  IfStatement _ thenBranch elseBranch ->
    1 + getListInstructionCount thenBranch + case elseBranch of
      Just stmts -> getListInstructionCount stmts + 1
      Nothing -> 0

getListInstructionCount :: [Statement] -> Int
getListInstructionCount stmts = sum (map getInstructionCount stmts)

type LineMap = Map Int Int

flattenStatement :: Int -> LineMap -> Statement -> (LineMap, [IntermediateInstruction])
flattenStatement currentOffset lineMap statement =
  let (Statement uniquePart lineNumber) = statement
      updatedLineMap = Map.insert lineNumber currentOffset lineMap
  in case uniquePart of
       GoToStatement targetLine -> (updatedLineMap, [JumpByLine targetLine Nothing])
       SwapStatement var1 var2 -> (updatedLineMap, [Final (SwapVariables var1 var2)])
       AssignmentStatement var expr -> (updatedLineMap, [Final (AssignVariable var expr)])
       ReturnStatement expr -> (updatedLineMap, [Final (ReturnValue expr)])
       IfStatement cond thenBranch elseBranch -> flattenIfStatement updatedLineMap (getInstructionCount statement) cond thenBranch elseBranch
  where
    flattenIfStatement updatedLineMap totalLength cond thenBranch elseBranch =
      let endOffset = currentOffset + totalLength
          (thenLineMap, thenInstructions) = flattenStatementList 1 updatedLineMap thenBranch
          (falseJumpOffset, elseLineMap, elseInstructions) = case flattenStatementList (3 + List.length thenInstructions) thenLineMap <$> elseBranch of
            Just (lm, instrs) -> (currentOffset + List.length thenInstructions + 2, lm, Final (unconditionalJump endOffset) : instrs)
            Nothing -> (endOffset, thenLineMap, [])
      in (elseLineMap, Final (conditionalJump falseJumpOffset (UnaryExpression (UnaryOp UnaryNot cond))) : thenInstructions ++ elseInstructions)

flattenStatementList :: Int -> LineMap -> [Statement] -> (LineMap, [IntermediateInstruction])
flattenStatementList currentOffset lineMap = List.foldl' step (lineMap, [])
  where
    step (lm, instrs) stmt =
      let (newLm, newInstrs) = flattenStatement (currentOffset + List.length instrs) lm stmt
       in (newLm, instrs ++ newInstrs)

flatten :: [Statement] -> [Instruction]
flatten statements =
  let (lineMap, intermediateInstructions) = flattenStatementList 0 Map.empty statements
   in map (finalizeInstruction lineMap) intermediateInstructions
  where
    finalizeInstruction lineMap (Final instr) = instr
    finalizeInstruction lineMap (JumpByLine targetLine cond) = Jump (lineMap Map.! targetLine) cond

envArgumentToAssignmentInstruction :: (VariableAccess, Expression) -> Instruction
envArgumentToAssignmentInstruction (var, expr) = AssignVariable var expr