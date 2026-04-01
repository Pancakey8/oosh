module Main where

import Bridge
import System.Console.Haskeline
import Control.Monad.IO.Class
import Syntax
import Text.Parsec
import IR

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "$ "
      case minput of
        Nothing -> return ()
        Just input -> do
          case runParser program () "shell" input of
            Right prog -> liftIO $ evalProgram $ compileProg prog
            Left err -> liftIO $ print err
          loop
