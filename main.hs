module Main where

import Text.ParserCombinators.Parsec hiding (spaces,
                                             (<|>))
import Control.Applicative hiding (many)
import Control.Monad
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?|^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
    string "#\\"
    x <- characterName <|> anyChar
    return $ Char x
  where
    -- Incomplete
    characterName =
            (string "space"     >> return ' ')
        <|> (string "backspace" >> return '\b')
        <|> (string "tab"       >> return '\t')
        <|> (string "newline"   >> return '\n')

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedCharacter <|> noneOf "\"")
    char '"'
    return $ String x
  where
    escapedCharacter = char '\\' >>
            (char  '"' >> return '"')
        <|> (char  't' >> return '\t')
        <|> (char  'n' >> return '\n')
        <|> (char  'r' >> return '\r')
        <|> (char '\\' >> return '\\')

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

{- excercise 1 - rewrite parseNumber using do-notation
 - and explicit sequencing with >>=

parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= return . Number . read

parseNumber'' :: Parser LispVal
parseNumber'' = do
    n <- many1 digit
    return $ Number (read n)

-- extra credit: applicative style
parseNumber''' :: Parser LispVal
parseNumber''' = (Number . read) <$> many1 digit
 -}

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseCharLiteral
        <|> parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))
