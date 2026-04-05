module Main where

import Bridge
import System.Console.Haskeline
import Control.Monad.IO.Class
import Syntax
import Text.Parsec
import IR

main :: IO ()
main = do
  state <- initProgram
  runInputT defaultSettings (loop state ""  False)
  where
    run :: ProgramStatePtr -> String -> IO ()
    run state inp =
      case runParser program () "" inp of
        Right prog ->
          let ir = compileProg prog
          in do
            mapM_ print ir
            evalProgram state ir
        Left err -> print err

    loop :: ProgramStatePtr -> String -> Bool -> InputT IO ()
    loop state acc isAccing = do
      minput <- getInputLine "$ "
      case minput of
        Nothing -> liftIO (freeProgram state)
        Just input ->
          case (input, isAccing) of
            ("#{", False) -> loop state "" True
            ("#}", True) -> liftIO (run state acc) >> loop state "" False
            (str, False) -> liftIO (run state str) >> loop state "" False
            (str, True) -> loop state (acc ++ str ++ "\n") True
