module IR where

import Syntax

data TemplatePart
  = TempExact String
  | TempVar String
  deriving (Show)

data PathPart
  = PathExact String
  | PathGlob
  | PathRecGlob
  | PathTmp [TemplatePart]
  deriving (Show)

data IRInstr
  = PushNum Double
  | PushTemplate [TemplatePart]
  | PushPath [PathPart]
  | PushFn String
  | LoadVar String
  | StoreVar String
  | DefineVar String
  | CallCommand Int
  | CallFunction Int
  | ApplyOp Operator
  deriving (Show)

compileProg :: [Statement] -> [IRInstr]
compileProg = concatMap compileStmt

compileStmt :: Statement -> [IRInstr]
compileStmt (ExprStmt ex) = compileExpr ex
compileStmt (DefVarStmt name ex) = compileExpr ex ++ [DefineVar name]
compileStmt (SetVarStmt name ex) = compileExpr ex ++ [StoreVar name]

compileExpr :: Expr -> [IRInstr]
compileExpr (LitExpr lit) = compileLit lit
compileExpr (BinaryExpr OpPipe l r) = error "TODO: Pipes"
compileExpr ex@(BinaryExpr OpJuxta l r) =
  case flattenJuxta ex of
    call@(LitExpr (VarLit name):args) -> concatMap compileExpr args ++ [PushFn name, CallFunction (length args)]
    _ -> error "Function call to non-function object"
compileExpr (BinaryExpr op l r) = compileExpr l ++ compileExpr r ++ [ApplyOp op]
compileExpr (CommandExpr name args) =
  map (PushTemplate . map templatifyContent) args ++ [commandName name, CallCommand (length args)]
  where
    commandName :: CommandName -> IRInstr
    commandName (Auto auto) = PushTemplate [TempExact auto]
    commandName (ExePath parts) = PushPath (map pathifyContent parts)
compileLit :: Literal -> [IRInstr]
compileLit (NumLit whole frac) = [PushNum (read $ whole ++ (if null frac
                                                            then ""
                                                            else '.' : frac))]
compileLit (StrLit contents) = [PushTemplate (map templatifyContent contents)]
compileLit (VarLit name) = [LoadVar name]
compileLit (PathLit parts) = [PushPath (map pathifyContent parts)]

templatifyContent :: StrContent -> TemplatePart
templatifyContent (Exact s) = TempExact s
templatifyContent (Subst (VarLit s)) = TempVar s
templatifyContent _ = error "Unhandled substitution syntax"

pathifyContent :: PathContent -> PathPart
pathifyContent (ExactPath s) = PathExact s
pathifyContent Glob = PathGlob
pathifyContent RecGlob = PathRecGlob
pathifyContent (StringPath contents) = PathTmp (map templatifyContent contents)

flattenJuxta :: Expr -> [Expr]
flattenJuxta = go []
  where
    go acc (BinaryExpr OpJuxta lhs rhs) = go (rhs : acc) lhs
    go acc headExpr = headExpr : acc
