module Main where

import Text.ParserCombinators.Parsec hiding (spaces,
                                             (<|>))
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Error
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> String
showError (UnboundVar m varname)   = m ++ ": " ++ varname
showError (BadSpecialForm m form)  = m ++ ": " ++ show form
showError (NotFunction m func)     = m ++ ": " ++ show func
showError (NumArgs ex found)       = "Expected " ++ show ex
                                  ++ " args; found values "
                                  ++ unwordsList found
showError (TypeMismatch ex found)  = "Invalid type: expected " ++ ex
                                  ++ ", found " ++ show found
showError (Parser err)             = "Parse error at " ++ show err

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has ocurred"
    strMsg = Default

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
parseNumber = (Number . read) <$> many1 digit

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
parseExpr = try parseCharLiteral
        <|> parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Atom _)               = return val
eval val@(Char _)               = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval val@(DottedList _ _)       = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval val@(List _)               = return val
eval badForm                    =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction
                        "Unrecognized primitive function args" f)
                     ($ args)
                     (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", return . isSymbol),
              ("string?", return . isString),
              ("number?", return . isNumber)]

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _]   = Bool True
isSymbol _          = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _          = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop f []    = throwError $ NumArgs 2 []
numericBinop f x@[_] = throwError $ NumArgs 2 x
numericBinop f xs    = (Number . foldl1 f) <$> mapM unpackNum xs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n)    = return n
unpackNum (String n)    =
    let parsed = reads n :: [(Integer, String)] in
        if null parsed
          then throwError $ TypeMismatch "number" $ String n
          else return $ fst (head parsed)
unpackNum (List [n])    = unpackNum n
unpackNum notNum        = throwError $ TypeMismatch "number" notNum

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
