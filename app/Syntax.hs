module Syntax where

import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String
import Control.Applicative (empty)

data StrContent
  = Exact String
  | Subst Literal
  deriving (Show)

data PathContent
  = ExactPath String
  | Glob
  | RecGlob
  | StringPath [StrContent]
  deriving (Show)

data Literal
  = NumLit String String
  | StrLit [StrContent]
  | VarLit String
  | PathLit [PathContent]
  | FunctionLit [Statement]
  deriving (Show)

data Operator = OpPipe | OpPlus | OpMinus | OpAst | OpSlash | OpJuxta
  deriving (Show)

data CommandName
  = ExePath [PathContent]
  | Auto String
  deriving (Show)

data Expr
  = LitExpr Literal
  | BinaryExpr Operator Expr Expr
  | GroupExpr Expr
  | CommandExpr CommandName [[StrContent]]
  deriving (Show)

data Statement
  = ExprStmt Expr
  | DefVarStmt String Expr
  | SetVarStmt String Expr
  deriving (Show)

numberLit :: Parser Literal
numberLit = do
  whole <- many1 digit
  frac <- (char '.' >> many1 digit) <|> pure ""
  pure $ NumLit whole frac

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

varLit :: Parser Literal
varLit = do
  _ <- char '$'
  name <- many1 (satisfy isIdentChar)
  pure (VarLit name)

stringLit :: Parser Literal
stringLit = do
  start <- char '"' <|> char '\''
  let dollarEscape = string "$$" >> pure '$'
      contentChar = satisfy (\c -> not (c == start || c == '$'))
      exactParse = Exact <$> many1 (try (dollarEscape <|> contentChar))
  contents <- many (exactParse <|> (Subst <$> varLit))
  _ <- char start
  pure $ StrLit contents

pathLit :: Parser Literal
pathLit = do
  _ <- char ':'
  let glob = try (string "**" >> pure RecGlob) <|> (char '*' >> pure Glob)
      exact = ExactPath <$> many1 (satisfy (\c -> not (isSpace c || c == '*' || c == '\'' || c == '"')))
      str = do
        StrLit contents <- stringLit
        pure $ StringPath contents
      path = many1 (glob <|> exact <|> str)
  PathLit <$> path

functionLit :: Parser Literal
functionLit = do
  _ <- symbol "{"
  _ <- statementSep
  stmts <- statement `sepEndBy` statementSep
  _ <- symbol "}"
  pure $ FunctionLit stmts

literal :: Parser Literal
literal = stringLit <|> numberLit <|> varLit <|> pathLit <|> functionLit

hSpaces :: Parser ()
hSpaces = skipMany (oneOf [' ', '\t'])

lexeme :: Parser a -> Parser a
lexeme p = hSpaces *> p <* hSpaces

symbol :: String -> Parser String
symbol s = lexeme (string s)

commandExpr :: Parser Expr
commandExpr = do
  cmdName <- getCmdName

  CommandExpr cmdName . filter (not . null) <$> getArgs
  where
    getCmdName :: Parser CommandName
    getCmdName = pathArg <|> autoArg

    getArgs :: Parser [[StrContent]]
    getArgs = manyTill getArg (try terminator)

    terminator :: Parser String
    terminator = lookAhead (symbol "|" <|> string "\n" <|> (eof >> pure "") <|> string ")" <|> string ";")

    isWordEnd :: Char -> Bool
    isWordEnd c = isSpace c || c `elem` ['\n', '|', '"', '\'', ')', ';']

    getArg :: Parser [StrContent]
    getArg = varArg <|> exactArg <|> quoteArg

    autoArg :: Parser CommandName
    autoArg = do
      first <- satisfy (\c -> isAlpha c || c `elem` permitted)
      name <- many (satisfy (\c -> isAlphaNum c || c `elem` permitted))
      pure $ Auto (first:name)
      where
        permitted = ['_', '.', '/', '-']

    pathArg :: Parser CommandName
    pathArg = do
      PathLit path <- pathLit
      pure (ExePath path)

    varArg :: Parser [StrContent]
    varArg = do
      _ <- lookAhead $ char '$'
      singleton . Subst <$> varLit

    exactArg :: Parser [StrContent]
    exactArg = do
      sp <- many (oneOf [' ', '\t'])
      if not (null sp)
        then pure []
        else do
          word <- many1 (satisfy (not . isWordEnd))
          pure [Exact word]

    quoteArg :: Parser [StrContent]
    quoteArg = do
      StrLit contents <- stringLit
      pure contents

nudExpr :: Bool -> Parser Expr
nudExpr allowCmd =
    (if allowCmd
      then commandExpr
      else empty)
    <|> (LitExpr <$> literal)
    <|> (GroupExpr <$> (symbol "(" *> ledExpr True 0 <* symbol ")"))

operators :: [(String, Operator, Int)]
operators = [("|", OpPipe, 10), ("+", OpPlus, 20), ("-", OpMinus, 20), ("*", OpAst, 30), ("/", OpSlash, 30)]

juxtaPrec :: Int
juxtaPrec = 40

ledExpr :: Bool -> Int -> Parser Expr
ledExpr allowCmd minPrec = do
  left <- lexeme (nudExpr allowCmd)
  go left
  where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    findOp :: String -> (String, Operator, Int)
    findOp s = head [info | info@(str, _, _) <- operators, str == s]

    go :: Expr -> Parser Expr
    go left = do
      maybeOp <- optionMaybe $ try $ lookAhead $ choice (map (try . symbol . fst3) operators)
      case maybeOp of
        Just opStr -> do
          let (_, opKind, prec) = findOp opStr
          if prec >= minPrec
            then do
              _ <- symbol opStr
              right <- ledExpr (case opKind of
                                  OpPipe -> True
                                  _ -> False) (prec + 1)
              go (BinaryExpr opKind left right)
            else pure left
        Nothing -> do
          next <- optionMaybe (lookAhead (lexeme (nudExpr False)))
          case next of
            Just _ ->
              if juxtaPrec >= minPrec
                then do
                  right <- ledExpr False (juxtaPrec + 1)
                  go (BinaryExpr OpJuxta left right)
                else pure left
            Nothing -> pure left

defVarStmt :: Parser Statement
defVarStmt = do
  name <- many1 (satisfy isIdentChar)
  _ <- symbol "="
  rhs <- ledExpr True 0
  pure (DefVarStmt name rhs)

setVarStmt :: Parser Statement
setVarStmt = do
  VarLit name <- varLit
  _ <- symbol "="
  rhs <- ledExpr True 0
  pure (SetVarStmt name rhs)

statementSep :: Parser ()
statementSep = skipMany (oneOf " \t\n\r;")

statement :: Parser Statement
statement = try setVarStmt <|> try defVarStmt <|> (ExprStmt <$> ledExpr True 0)

program :: Parser [Statement]
program = do
  statementSep
  prog <- statement `sepEndBy` statementSep
  _ <- eof
  pure prog
