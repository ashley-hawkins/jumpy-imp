-- Just interprets the command line arguments and runs the program and outputs the result.
{-# LANGUAGE DeriveGeneric #-}

module Run where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Flatten
import GHC.Generics (Generic)
import Interpreter
import Parser
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

newtype RunSuccess = RunSuccess Environment
  deriving (Show, Generic)

instance ToJSON RunSuccess

instance FromJSON RunSuccess

data RunFailure = EnvironmentParseError String | ProgramParseError String | EnvironmentSetupError Interpreter.Error | MainProgramError Interpreter.Error
  deriving (Show, Generic)

instance ToJSON RunFailure

instance FromJSON RunFailure

type RunResult = Either RunFailure RunSuccess

makeEnvironmentSetupInstruction :: String -> Either ParserError Instruction
makeEnvironmentSetupInstruction str = do
  (var, expr) <- stringToEnvArgument str
  return $ envArgumentToAssignmentInstruction (var, expr)

makeEnvironmentSetupProgram :: [String] -> Either ParserError [Instruction]
makeEnvironmentSetupProgram = mapM makeEnvironmentSetupInstruction

runPure :: String -> String -> [String] -> RunResult
runPure fileName mainProgramText environmentInitialisers = do
  environmentSetupProgram <- first (EnvironmentParseError . errorBundlePretty) $ makeEnvironmentSetupProgram environmentInitialisers
  environmentSetupResult <- first EnvironmentSetupError $ environment <$> interpretProgram environmentSetupProgram
  mainProgramStatements <- first (ProgramParseError . errorBundlePretty) $ parseFileToProgram fileName (fromString mainProgramText)
  mainProgramResult <- first MainProgramError $ interpretProgramWithEnv (flatten mainProgramStatements) environmentSetupResult
  return $ RunSuccess $ environment mainProgramResult

run :: IO RunResult
run = do
  args <- getArgs
  let fileName = head args
      environmentInitialisers = drop 1 args
   in do
        mainProgramText <- readFile fileName
        return $ runPure fileName mainProgramText environmentInitialisers

runToJson :: IO ()
runToJson = do
  runResult <- run
  BL.putStrLn $ encode runResult