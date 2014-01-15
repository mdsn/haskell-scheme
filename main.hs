{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Text.ParserCombinators.Parsec hiding (spaces,
                                             (<|>))
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Data.Maybe (isJust, isNothing)
import System.Environment
import System.IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: Maybe String
                    , body    :: [LispVal]
                    , closure :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
    show = showVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker a1
    unpacked2 <- unpacker a2
    return $ unpacked1 == unpacked2
  `catchError` const (return False)


--  IORef environment

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var)
                     (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


--  Equality functions

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params  = args
              , vararg  = varargs
              , body    = body
              , closure = env }) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just a  -> " . " ++ a) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)                        = return val
eval env val@(Char _)                          = return val
eval env val@(Number _)                        = return val
eval env val@(Bool _)                          = return val
eval env val@(DottedList _ _)                  = return val
eval env (Atom id)                             = getVar env id
eval env (List [Atom "quote", val])            = return val

eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise  -> eval env conseq

eval env (List (Atom "cond" : clauses))        = run clauses
  where
    run :: [LispVal] -> IOThrowsError LispVal
    run (List (Atom "else" : expr) : []) = last <$> mapM (eval env) expr
    run (List (test : expr) : xs)        = do
        result <- eval env test
        case result of
            Bool True -> last <$> mapM (eval env) expr
            otherwise -> run xs
    run []                               = throwError $ Default
        "No clause evaluated to true in cond statement"

eval env (List (Atom "case" : key : clauses))  = do
    keyValue <- eval env key
    run keyValue clauses
  where
    run :: LispVal -> [LispVal] -> IOThrowsError LispVal
    run _ []                               = throwError $ Default
        "No clause evaluated to true in case statement"
    run k (List (Atom "else" : expr) : []) = last <$> mapM (eval env) expr
    run k (List (List datum : expr) : xs)  =
        if k `oneOf` datum then last <$> mapM (eval env) expr
        else run k xs
    
    oneOf :: LispVal -> [LispVal] -> Bool
    oneOf _ []     = False
    oneOf k (x:xs) = case safeEqv k x of
                        Bool True -> True
                        Bool False -> oneOf k xs

eval env (List [Atom "set!", Atom var, form])  =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

eval env (List (function : args)) = do
    f <- eval env function
    argVals <- mapM (eval env) args
    apply f argVals

eval env badForm = throwError $
                   BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args                  = liftThrows $ f args
apply (Func params varargs body closure) args = 
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else liftIO (bindVars closure $ zip params args)
            >>= bindVarArgs varargs
            >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing      -> return env

apply (IOFunc f) args = f args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [f, List args] = apply f args
applyProc (f : args) = apply f args

makeFunc varargs env params body =
    return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv
                >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
                                ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, f) = (var, constructor f)

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

-- IO Primitives

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- doesn't throw error since it only accepts two arguments
-- returns false in case of error
safeEqv :: LispVal -> LispVal -> LispVal
safeEqv a b = case eqv [a, b] of
                Left _           -> Bool False
                Right (Bool val) -> Bool val

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
                      liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne (filename : args) = do
    env <- primitiveBindings >>=
           flip bindVars [("args", List $ map String args)]
    runIOThrows (liftM show $ eval env loadVal) >>= hPutStrLn stderr
  where loadVal = List [Atom "load", String filename]

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit")
                                       (readPrompt "lisp> ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl
                 else runOne args
