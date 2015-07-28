module Parser (readExpr, readExprList) where
import Value
import Error
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf ":!$%&|*+-/;<=?>@^_~#"

parseEscape :: Bool -> Parser Char
parseEscape str =
  do
    char '\\'
    c <- oneOf filter
    return $ case c of
      '\\' -> '\\'
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      '\'' -> '\''
      '"' -> '"'

    where filter = if str then "\\nrt\"" else "\\nrt'"

parseString :: Parser YmirValue
parseString =
  do
    char '"'
    x <- many (parseEscape True <|> noneOf "\"")
    char '"'
    return (String x)

parseAtom :: Parser YmirValue
parseAtom =
  do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
      "true" -> Bool True
      "false" -> Bool False
      otherwise -> Atom atom

parseNumber :: Parser YmirValue
parseNumber = liftM (Number . read) $ many1 digit

parseChar =
  do
    char '\''
    c <- parseEscape False <|> noneOf "'"
    char '\''
    return (Char c)

parseList :: Parser YmirValue
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser YmirValue
parseDottedList =
  do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)

parseQuoted :: Parser YmirValue
parseQuoted =
  do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser YmirValue
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> ((try parseChar) <|> parseQuoted)
  <|> do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x

readOrThrow :: String -> Parser a -> String -> ThrowsError a
readOrThrow filename parser input = case parse parser filename input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError YmirValue
readExpr = readOrThrow "ymir" parseExpr

readExprList filename = readOrThrow filename (endBy parseExpr spaces)
