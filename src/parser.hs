module Parser (readExpr) where
import Value
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/;<=?>@^_~#"

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
  <|> parseChar
  <|> parseQuoted
  <|> do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x

readExpr :: String -> String
readExpr input =
  case parse parseExpr "ymir" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
