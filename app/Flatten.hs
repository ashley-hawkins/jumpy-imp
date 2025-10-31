module Flatten where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Language (Expression (UnaryExpression), Statement (Statement), StatementUniquePart (AssignmentStatement, GoToStatement, IfStatement, SwapStatement), UnaryOp (UnaryOp), UnaryOperator (UnaryNot), VariableAccess)
import Language qualified

data Instruction
  = Jump Int (Maybe Expression)
  | SwapVariables VariableAccess VariableAccess
  | AssignVariable VariableAccess Expression
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
        IfStatement cond thenBranch elseBranch -> flattenIfStatement updatedLineMap (getInstructionCount statement) cond thenBranch elseBranch
  where
    flattenIfStatement updatedLineMap totalLength cond thenBranch elseBranch =
      let endOffset = currentOffset + totalLength
          (thenLineMap, thenInstructions) = flattenStatementList 1 updatedLineMap thenBranch
          (falseJumpOffset, elseLineMap, elseInstructions) = case flattenStatementList (1 + List.length thenInstructions) thenLineMap <$> elseBranch of
            -- Extra jump to get past the else branch when reaching the end of the "then" part.
            -- False jump offset is set to the start of the else branch's instructions.
            Just (lm, instrs) -> (currentOffset + List.length thenInstructions + 1, lm, Final (unconditionalJump endOffset) : instrs)
            -- If there's no else branch, we just jump to the end when the condition is false.
            Nothing -> (endOffset, thenLineMap, [])
       in (elseLineMap, Final (conditionalJump falseJumpOffset (UnaryExpression (UnaryOp UnaryNot cond))) : thenInstructions ++ elseInstructions)

flattenStatementList :: Int -> LineMap -> [Statement] -> (LineMap, [IntermediateInstruction])
flattenStatementList currentOffset lineMap = List.foldl' step (lineMap, [])
  where
    step (lm, instrs) stmt =
      let (newLm, newInstrs) = flattenStatement (currentOffset + List.length instrs) lm stmt
       in (newLm, instrs ++ newInstrs)