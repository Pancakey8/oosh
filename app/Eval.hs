module Eval where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State qualified as S
import Data.List
import Syntax

data Value
  = NumVal Float
  | StrVal String
  | FileVal String
  | NullVal
  deriving (Show)

data EvalState = EvalState {globals :: [(String, Value)], locals :: [(String, Value)]}
  deriving (Show)

data EvalException
  = VarNotDefined String
  | RedefineExisting String
  deriving (Show)

instance Exception EvalException

type EvalM a = S.StateT EvalState IO a

evalStmt :: Statement -> EvalM Value
evalStmt stmt =
  case stmt of
    DefVarStmt name expr ->
      do
        state@EvalState {locals} <- S.get
        if any ((== name) . fst) locals
          then S.lift $ throwIO $ RedefineExisting name
          else do
            val <- evalExpr expr
            S.put state {locals = (name, val) : locals}
            pure NullVal
    SetVarStmt name expr ->
      do
        state@EvalState {globals, locals} <- S.get
        let hasAny = any ((== name) . fst)
            dropVar = filter ((/= name) . fst)
        if hasAny locals
          then do
            val <- evalExpr expr
            S.put state {locals = (name, val) : dropVar locals}
            pure NullVal
          else
            if hasAny globals
              then do
                val <- evalExpr expr
                S.put state {globals = (name, val) : dropVar globals}
                pure NullVal
              else S.lift $ throwIO $ VarNotDefined name
    ExprStmt expr -> evalExpr expr

evalExpr :: Expr -> EvalM Value
evalExpr expr =
  case expr of
    LitExpr lit -> evalLit lit
    CommandExpr exe args -> pure NullVal
    BinaryExpr op left right ->
      case op of
        OpPlus -> pure NullVal
        OpMinus -> pure NullVal
        OpAst -> pure NullVal
        OpSlash -> pure NullVal
        OpJuxta -> pure NullVal
        OpPipe -> pure NullVal

getVar :: String -> EvalM (Maybe Value)
getVar name = do
  EvalState {globals, locals} <- S.get
  let findVar = find ((== name) . fst)
  pure $ snd <$> (findVar locals <|> findVar globals)

evalLit :: Literal -> EvalM Value
evalLit lit =
  case lit of
    NumLit whole frac ->
      pure $
        NumVal
          ( read $
              whole
                ++ if null frac
                  then ""
                  else '.' : frac
          )
    StrLit contents -> pure NullVal
    VarLit name -> do
      maybeVal <- getVar name
      case maybeVal of
        Just value -> pure value
        Nothing -> S.lift $ throwIO (VarNotDefined name)
    PathLit paths -> pure NullVal
