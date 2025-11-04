-- Just interprets the command line arguments and runs the program and outputs the result.
module Main where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Flatten
import Interpreter
import Parser
import System.Environment (getArgs)
import Text.Pretty.Simple
import Text.Megaparsec (errorBundlePretty)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
      environmentSetupProgram = mapMaybe ((fmap envArgumentToAssignmentInstruction . eitherToMaybe) . stringToEnvArgument) (drop 1 args)
   in do
        mainProgramText <- readFile fileName
        let environmentSetupResult = case interpretProgram environmentSetupProgram of
              Right state -> environment state
              Left err -> error $ "Failed to interpret environment setup: " ++ show err
            mainProgramStatements = case parseFileToProgram fileName (fromString mainProgramText) of
              Right prog -> prog
              Left err -> error $ "Failed to parse main program: " ++ errorBundlePretty err
            mainProgram = flatten mainProgramStatements
            mainProgramResult = case interpretProgramWithEnv mainProgram environmentSetupResult of
              Right state -> state
              Left err -> error $ "Failed to interpret main program: " ++ show err
         in do
              --   pPrint mainProgramStatements
              --   pPrint mainProgram
              putStrLn $ "Environment at program exit: " ++ show (environment mainProgramResult)
              putStrLn $ case Map.lookup "$return" (environment mainProgramResult) of
                Just val -> "Return value: " ++ show val
                Nothing -> "Program terminated without a return value"