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
  runInputT defaultSettings (loop state)
  where
    loop :: ProgramStatePtr -> InputT IO ()
    loop state = do
      minput <- getInputLine "$ "
      case minput of
        Nothing -> return ()
        Just input -> do
          case runParser program () "shell" input of
            Right prog -> liftIO $ evalProgram state $ compileProg prog
            Left err -> liftIO $ print err
          loop state
