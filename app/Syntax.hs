module Syntax where

import Data.Char
import Text.Parsec
import Text.Parsec.String

data StrContent
  = Exact String
  | Subst Literal
  deriving (Show)

data Literal
  = NumLit String String
  | StrLit [StrContent]
  | VarLit String
  | KwLit String
  deriving (Show)

data Operator = OpPipe | OpPlus | OpMinus | OpAst | OpSlash | OpJuxta
  deriving (Show)

data Expr
  = LitExpr Literal
  | BinaryExpr Operator Expr Expr
  deriving (Show)

data Statement
  = ExprStmt Expr
  | SetVarStmt String Expr
  deriving (Show)

numberLit :: Parser Literal
numberLit = do
  whole <- many1 digit
  frac <- (char '.' >> many1 digit) <|> pure ""
  pure $ NumLit whole frac

reservedMinimal :: [Char]
reservedMinimal = ['\'', '"', '(', ')', '[', ']', '$']

reservedExtra :: [Char]
reservedExtra = ['+', '-', '*', '/', '|']

identCharStart :: Char -> Bool
identCharStart c = not (isSpace c || isDigit c || c `elem` reservedMinimal || c `elem` reservedExtra)

identChar :: Char -> Bool
identChar c = identCharStart c || isDigit c

kwCharStart :: Char -> Bool
kwCharStart c = not (isSpace c || isDigit c || c `elem` reservedMinimal)

kwChar :: Char -> Bool
kwChar c = kwCharStart c || isDigit c

varLit :: Parser Literal
varLit = do
  _ <- char '$'
  first <- satisfy identCharStart
  name <- many (satisfy identChar)
  pure $ VarLit (first : name)

kwLit :: Parser Literal
kwLit = do
  first <- satisfy kwCharStart
  name <- many (satisfy kwChar)
  pure $ KwLit (first : name)

stringLit :: Parser Literal
stringLit = do
  start <- char '"' <|> char '\''
  let dollarEscape = string "$$" >> pure '$'
      contentChar = satisfy (\c -> not (c == start || c == '$'))
      exactParse = Exact <$> many1 (try (dollarEscape <|> contentChar))
  contents <- many (exactParse <|> (Subst <$> varLit))
  _ <- char start
  pure $ StrLit contents

literal :: Parser Literal
literal = stringLit <|> numberLit <|> varLit <|> kwLit

hSpaces :: Parser ()
hSpaces = skipMany (oneOf [' ', '\t'])

lexeme :: Parser a -> Parser a
lexeme p = hSpaces *> p <* hSpaces

symbol :: String -> Parser String
symbol s = lexeme (string s)

nudExpr :: Parser Expr
nudExpr =
  (LitExpr <$> literal)
    <|> (symbol "(" *> ledExpr 0 <* symbol ")")

operators :: [(String, Operator, Int)]
operators = [("|", OpPipe, 10), ("+", OpPlus, 20), ("-", OpMinus, 20), ("*", OpAst, 30), ("/", OpSlash, 30)]

juxtaPrec :: Int
juxtaPrec = 30

ledExpr :: Int -> Parser Expr
ledExpr minPrec = do
  left <- lexeme nudExpr
  go left
  where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    findOp :: String -> (String, Operator, Int)
    findOp s = head [info | info@(str, _, _) <- operators, str == s]

    go :: Expr -> Parser Expr
    go left = do
      maybeOp <- optionMaybe $ try $ do
        hSpaces
        opStr <- choice (map (try . string . fst3) operators)
        _ <- satisfy isSpace
        return opStr
      case maybeOp of
        Just opStr -> do
          let (_, opKind, prec) = findOp opStr
          if prec >= minPrec
            then do
              right <- ledExpr (prec + 1)
              go (BinaryExpr opKind left right)
            else pure left
        Nothing -> do
          next <- optionMaybe (lookAhead (lexeme nudExpr))
          case next of
            Just _ ->
              if juxtaPrec >= minPrec
                then do
                  right <- ledExpr juxtaPrec
                  go (BinaryExpr OpJuxta left right)
                else pure left
            Nothing -> pure left

setVarStmt :: Parser Statement
setVarStmt = do
  first <- satisfy identCharStart
  name <- many (satisfy identChar)
  _ <- symbol "="
  rhs <- ledExpr 0
  pure (SetVarStmt (first : name) rhs)

statement :: Parser Statement
statement = try setVarStmt <|> (ExprStmt <$> ledExpr 0)
