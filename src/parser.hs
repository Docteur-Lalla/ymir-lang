module Parser (readExpr, readExprList) where
import Value
import Error
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf ":!$%&|*+-/<=?>@^_~#"

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

parseInteger :: Parser YmirValue
parseInteger = liftM (Number . Integer . read) $ (parseNegative <|> many1 digit)
  
  where
    parseNegative =
      do
        m <- char '-'
        i <- many1 digit
        return (m:i)

parseFloat :: Parser YmirValue
parseFloat = liftM (Number . Float . read) $
  do
    parseNegative
    <|> parsePositive
  
  where
    parsePositive =
      do
        i <- many1 digit
        char '.'
        d <- many1 digit
        return (i ++ "." ++ d)
    parseNegative =
      do
        m <- char '-'
        i <- parsePositive
        return (m:i)

parseNumber :: Parser YmirValue
parseNumber = (try parseFloat) <|> (try parseInteger)

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
  parseNumber
  <|> parseAtom
  <|> parseString
  <|> ((try parseChar) <|> parseQuoted)
  <|> do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x

readOrThrow :: String -> Parser a -> String -> ThrowsError a
readOrThrow filename parser input =
  case parse skipComments filename input of
    Left err -> throwError $ Parser err
    Right val ->
      case parse parser filename val of
        Left err -> throwError $ Parser err
        Right val -> return val

readExpr :: String -> ThrowsError YmirValue
readExpr = readOrThrow "ymir" parseExpr

readExprList filename = readOrThrow filename (endBy parseExpr spaces)

comment :: Parser ()
comment = char ';' >> manyTill anyChar newline >> return ()

skipComments :: Parser String
skipComments =
  do
    optional comment
    codes <- sepBy notComment comment
    optional comment
    return $ foldl (\x y -> x `op` y) "" codes

  where
    notComment = manyTill anyChar (lookAhead (comment <|> eof))
    "" `op` xs = xs
    x `op` xs = x ++ "\n" ++ xs
