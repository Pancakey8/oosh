module Main where

import Bridge
import System.Console.Haskeline
import Control.Monad.IO.Class
import Syntax

-- main :: IO ()
-- main = runInputT defaultSettings loop
--   where
--     loop :: InputT IO ()
--     loop = do
--       minput <- getInputLine "$ "
--       case minput of
--         Nothing -> return ()
--         Just input -> do
--           -- liftIO $ eval input
--           loop

main :: IO ()
main = stringifyLiteral (StrLit [Exact "hey, ", Subst (VarLit "foo")])
