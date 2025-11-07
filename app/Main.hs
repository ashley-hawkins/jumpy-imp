-- Just interprets the command line arguments and runs the program and outputs the result.
module Main where

import Data.Map qualified as Map
import Run (RunFailure (..), RunSuccess (..), run)

main :: IO ()
main = do
  runResult <- run
  case runResult of
    Left (EnvironmentParseError err) -> error $ "Failed to parse environment setup: " ++ err
    Left (EnvironmentSetupError err) -> error $ "Failed to interpret environment setup: " ++ show err
    Left (ProgramParseError err) -> error $ "Failed to parse main program: " ++ err
    Left (MainProgramError err) -> error $ "Failed to interpret main program: " ++ show err
    Right (RunSuccess env) -> do
      putStrLn $ "Environment at program exit: " ++ show env
      putStrLn $ case Map.lookup "$return" env of
        Just val -> "Return value: " ++ show val
        Nothing -> "Program terminated without a return value"