{-# LANGUAGE ExistentialQuantification #-}
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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker a1
    unpacked2 <- unpacker a2
    return $ unpacked1 == unpacked2
  `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal as@[List a1, List a2] = listEqv as pairEqv
  where
    pairEqv (a1, a2) = case equal [a1, a2] of
                        Left err -> False
                        Right (Bool val) -> val
equal [a1, a2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals a1 a2)
                        [AnyUnpacker unpackNum
                        ,AnyUnpacker unpackStr
                        ,AnyUnpacker unpackBool]
    eqvEquals <- eqv [a1, a2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
showError (Default s)              = s

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
eval val@(String _)                        = return val
eval val@(Atom _)                          = return val
eval val@(Char _)                          = return val
eval val@(Number _)                        = return val
eval val@(Bool _)                          = return val
eval val@(DottedList _ _)                  = return val
eval (List [Atom "quote", val])            = return val

eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq

eval (List (Atom "cond" : clauses))        = run clauses
  where
    run :: [LispVal] -> ThrowsError LispVal
    run (List (Atom "else" : expr) : []) = last <$> mapM eval expr
    run (List (test : expr) : xs)        = do
        result <- eval test
        case result of
            Bool True -> last <$> mapM eval expr
            otherwise -> run xs
    run []                               = throwError $ Default
        "No clause evaluated to true in cond statement"

eval (List (Atom "case" : key : clauses))  = do
    keyValue <- eval key
    run keyValue clauses
  where
    run :: LispVal -> [LispVal] -> ThrowsError LispVal
    run _ []                          = throwError $ Default
        "No clause evaluated to true in case statement"
    run k (List (List datum:expr):xs) =
        if k `oneOf` datum then last <$> mapM eval expr
        else run k xs
    
    oneOf :: LispVal -> [LispVal] -> Bool
    oneOf k []     = False
    oneOf k (x:xs) = case eqv [k, x] of
                        Left err -> False
                        Right val -> case val of
                            Bool True -> True
                            Bool False -> oneOf k xs

eval (List (Atom func : args))             = mapM eval args >>= apply func
eval val@(List _)                          = return val

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
              ("number?", return . isNumber),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", equal)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a1, Bool a2]      = return $ Bool $ a1 == a2
eqv [Number a1, Number a2]  = return $ Bool $ a1 == a2
eqv [String a1, String a2]  = return $ Bool $ a1 == a2
eqv [Atom a1, Atom a2]      = return $ Bool $ a1 == a2
eqv [DottedList xs x, DottedList ys y] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv as@[List a1, List a2]              = listEqv as eqvPair
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                            Left err         -> False
                            Right (Bool val) -> val
eqv [_, _]                      = return $ Bool False
eqv badArgList                  = throwError $ NumArgs 2 badArgList

listEqv :: [LispVal]
        -> ((LispVal, LispVal) -> Bool)
        -> ThrowsError LispVal
listEqv [List a1, List a2] f = return $ Bool $ (length a1 == length a2) &&
                                    all f (zip a1 a2)

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]             = return $ List [x]
cons [x, List xs]             = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop f op args = if length args /= 2
                      then throwError $ NumArgs 2 args
                      else do left <- f $ head args
                              right <- f $ args !! 1
                              return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

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

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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
