module Main where
import Control.Monad
import Data.Char
import Numeric
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Decimal Double


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

nonQuote :: Parser Char
nonQuote = noneOf "\""

escapeCodes :: Parser Char
escapeCodes = oneOf "nrt\\"

escapedChar :: Parser Char
escapedChar = do
    escaped <- char '\\' >> escapeCodes
    return $ case escaped of
            'n'  -> '\n'
            'r'  -> '\r'
            't'  -> '\t'
            '\\' -> '\\'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (nonQuote <|> escapedChar)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many(letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom

parseNum :: Parser String
parseNum = liftM read $ many1 digit

parseNumFromBase :: String -> (String -> Integer) -> Parser LispVal
parseNumFromBase chars f = do
    n <- char '#' >> oneOf chars >> parseNum
    return $ Number $ f n

parseBin :: Parser LispVal
parseBin = parseNumFromBase "bB" (fst . head . readInt 2 (`elem` "01") digitToInt)

parseOct :: Parser LispVal
parseOct = parseNumFromBase "oO" (fst . head . readOct)

parseHex :: Parser LispVal
parseHex = parseNumFromBase "xX" (fst . head . readHex)

parseDec :: Parser LispVal
parseDec = parseNumFromBase "dD" read

parseInt :: Parser LispVal
parseInt = parseNum >>= \n -> return $ Number (read n)

parseNumber :: Parser LispVal
parseNumber = try parseDecimal
                <|> parseBin
                <|> parseOct
                <|> parseHex
                <|> parseDec
                <|> parseInt

parseCharacter :: Parser LispVal
parseCharacter = do
    ident <- char '#' >> char '\\' >> many(letter)
    let upperIdent = map toUpper ident
    return $ Character $ case (upperIdent, ident) of
                            ("NEWLINE", _)        -> '\n'
                            ("SPACE", _)          -> ' '
                            ("TAB", _)            -> '\t'
                            (_, id)                -> head id

parseDecimal :: Parser LispVal
parseDecimal = do
    intpart <- many(digit)
    fracPart <- char '.' >> many(digit <|> letter)
    return $ Decimal $ (fst . head . readFloat) (intpart ++ ['.'] ++ fracPart)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Found Value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr