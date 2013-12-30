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
             | Bool Bool

instance Show LispVal where
    show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom a)   = a
showVal (Number n) = show n
showVal (Bool False) = "#f"
showVal (Bool True)  = "#t"
showVal (Char c)     = show c
showVal (List xs)    = "(" ++ unwordsList xs ++ ")"
showVal (DottedList x xs) = "(" ++ unwordsList x 
                       ++ " . " ++ showVal xs ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

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

parseExpr :: Parser LispVal
parseExpr = parseCharLiteral
        <|> parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Atom _)               = val
eval val@(Char _)               = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval val@(DottedList _ _)       = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args
eval val@(List _)               = val

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> LispVal
numericBinop f = Number . foldl1 f . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n)    = n
unpackNum (String n)    =
    let parsed = reads n :: [(Integer, String)] in
        if null parsed
          then 0
          else fst $ head parsed
unpackNum (List [n])    = unpackNum n
unpackNum _             = 0

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
