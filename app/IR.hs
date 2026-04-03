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
  | PushFn [IRInstr]
  | LoadVar String
  | StoreVar String
  | DefineVar String
  | CallCommand Int
  | CallFunction Int
  | ApplyOp Operator
  | PipeTo
  deriving (Show)

compileProg :: [Statement] -> [IRInstr]
compileProg = concatMap compileStmt

compileStmt :: Statement -> [IRInstr]
compileStmt (ExprStmt ex) = compileExpr ex
compileStmt (DefVarStmt name ex) = compileExpr ex ++ [DefineVar name]
compileStmt (SetVarStmt name ex) = compileExpr ex ++ [StoreVar name]

compileExpr :: Expr -> [IRInstr]
compileExpr (LitExpr lit) = compileLit lit
compileExpr (BinaryExpr OpPipe l r) =
  compileExpr l ++ [PipeTo] ++ compileExpr r
compileExpr ex@(BinaryExpr OpJuxta _ _) =
  case flattenJuxta ex of
    (callee:args) -> concatMap compileExpr args ++ compileExpr callee ++  [CallFunction (length args)]
    _ -> error "Juxta on a single node (?)"
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
compileLit (FunctionLit body) = [PushFn (compileProg body)]

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
