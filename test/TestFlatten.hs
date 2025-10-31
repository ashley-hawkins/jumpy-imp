module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List qualified as List
import Data.Map qualified as Map

import Language
import Flatten

mkAssign :: Int -> String -> Statement
mkAssign ln name =
  Statement
    { uniquePart = AssignmentStatement {variable = VariableAccess name, value = LiteralExpression (LiteralFloat 1.0)},
      lineNumber = ln
    }

mkSwap :: Int -> String -> String -> Statement
mkSwap ln a b =
  Statement {uniquePart = SwapStatement {var1 = VariableAccess a, var2 = VariableAccess b}, lineNumber = ln}

mkGoto :: Int -> Int -> Statement
mkGoto ln target = Statement {uniquePart = GoToStatement {goToLine = target}, lineNumber = ln}

mkIf :: Int -> Expression -> [Statement] -> Maybe [Statement] -> Statement
mkIf ln cond thenBranch elseBranch =
  Statement
    { uniquePart = IfStatement {condition = cond, thenBranch = thenBranch, elseBranch = elseBranch},
      lineNumber = ln
    }

shouldMatchCount :: Statement -> Assertion
shouldMatchCount stmt = do
  let (_, instrs) = flattenStatement 0 Map.empty stmt
  List.length instrs @?= getInstructionCount stmt

tests :: TestTree
tests = testGroup "flattenStatement vs getInstructionCount"
  [ testCase "assignment" $ shouldMatchCount (mkAssign 1 "x"),
    testCase "swap" $ shouldMatchCount (mkSwap 2 "a" "b"),
    testCase "goto" $ shouldMatchCount (mkGoto 3 42),
    testCase "if without else" $ shouldMatchCount (mkIf 5 (LiteralExpression (LiteralBool True)) [mkAssign 4 "y"] Nothing),
    testCase "if with else" $ shouldMatchCount (mkIf 8 (LiteralExpression (LiteralBool False)) [mkAssign 6 "y"] (Just [mkAssign 7 "z"])),
    testCase "nested ifs" $ shouldMatchCount (let innerThen = mkAssign 11 "i"; innerIf = mkIf 10 (LiteralExpression (LiteralBool True)) [innerThen] Nothing in mkIf 9 (LiteralExpression (LiteralBool False)) [innerIf] Nothing)
  ]

main :: IO ()
main = defaultMain tests
