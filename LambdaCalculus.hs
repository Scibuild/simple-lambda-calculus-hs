module Main where

import qualified Data.Bifunctor
import Control.Applicative 
import Control.Monad (forever, foldM)
import qualified Data.Map as Map 
import System.IO
import System.Exit (exitSuccess)
import Parser
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

data LambdaExpr = 
    T String 
  | L String LambdaExpr 
  | A LambdaExpr LambdaExpr
  | C LambdaExpr VEnv -- 'closure' that just captures variables bound in a scope without substituting
  deriving Show

showLambda :: LambdaExpr -> String
showLambda = showLambda' 0

showLambda' :: Int -> LambdaExpr -> String
showLambda' 20 _ = "..."
showLambda' n (T s) = s
showLambda' n (L s e) = "\\" ++ s ++ "." ++ showLambda' (n + 1) e

showLambda' n (C e m) = "(" ++ showLambda' n e ++ " [" ++ intercalate ", " (map (\(s, e) -> s ++ " := " ++ showLambda' (n + 1) e) $ Map.toList m) ++ "]" ++ ")"

showLambda' n (A (T t) (T u)) = t ++ " " ++ u
showLambda' n (A (T t) e) = t ++ " (" ++ showLambda' n e ++ ")"

showLambda' n (A l@(L _ _) l2@(L _ _)) = "(" ++ showLambda' n l ++") (" ++ showLambda' n l2 ++ ")"
showLambda' n (A l@(L _ _) c@(C _ _)) = "(" ++ showLambda' n l ++") (" ++ showLambda' n c ++ ")"
showLambda' n (A l@(L _ _) e) = "(" ++ showLambda' n l ++")" ++ showLambda' n e

showLambda' n (A c@(C _ _) l@(L _ _)) = "(" ++ showLambda' n c ++") (" ++ showLambda' n l ++ ")"
showLambda' n (A c@(C _ _) c2@(C _ _)) = "(" ++ showLambda' n c ++") (" ++ showLambda' n c2 ++ ")"
showLambda' n (A c@(C _ _) e) = "(" ++ showLambda' n c ++")" ++ showLambda' (n+1) e

showLambda' n (A a@(A _ _) (T u)) = showLambda' n a ++ " " ++ u
showLambda' n (A a@(A _ _) e) = showLambda' n a ++ " (" ++ showLambda' (n+1) e ++ ")"


betaReduce :: LambdaExpr -> VEnv -> LambdaExpr
betaReduce (T s) env = 
  case Map.lookup s env of
    Just l -> l 
    -- Nothing -> T (s ++ ":(failedLookup " ++ show env ++ ")")
    Nothing -> T s
betaReduce l@(L v e) env = L v (betaReduce e (Map.delete v env))
betaReduce (A f x) env = A (betaReduce f env) (betaReduce x env)
betaReduce (C v m) env = betaReduce v (Map.union m env) 


evaluate :: LambdaExpr -> VEnv -> LambdaExpr
evaluate (T s) env = 
  case Map.lookup s env of
    Just l -> l 
    Nothing -> T s
    -- Nothing -> T (s ++ ":(failedLookup " ++ show env ++ ")")
evaluate (L v e) env = L v (C e (Map.delete v env))
evaluate (C (L v e) m) env = L v $ C e m
evaluate (C e m) env = evaluate e (Map.union m env)
evaluate (A f x) env = 
  case evaluate f env of
    L v c@(C e m) -> evaluate ( C e $ Map.insert v evalX m ) env  
    L v e -> evaluate (C e $ Map.fromList [(v, evalX)]) env 
    T s -> A (T s) evalX
    A l r -> A (A l r) evalX
    C v m -> T "Applying to closure" 
  where evalX = evaluate x env
    
parseAndEval :: String -> Maybe LambdaExpr
parseAndEval s = parse parseExpr s >>= \x -> return $ evaluate (fst x) Map.empty

type LParser = Parser Char LambdaExpr
type VEnv = Map.Map String LambdaExpr

parseLambda :: LParser
parseLambda = do
  match '\\'
  spaces
  (T input) <- parseTerm
  spaces
  match '.'
  spaces
  L input <$> parseExpr

parseTerm :: LParser
parseTerm = T <$> some ( satisfy (not . (`elem` "()\\. $")))

parseNonAppl = 
  spaces >>
     (parseLambda
  <|> parseTerm 
  <|> parseDollarExpr
  <|> parseBracketExpr)

toLeftRecursive :: LambdaExpr -> [LambdaExpr] -> LambdaExpr
toLeftRecursive = foldl A 

parseApplication = do
  toLeftRecursive <$> parseNonAppl <*> many parseNonAppl

parseDollarExpr = do
  match '$'
  spaces
  parseExpr

parseBracketExpr = do
    match '('
    spaces
    expr <- parseExpr
    spaces
    match ')'
    return expr


parseExpr :: LParser
parseExpr = do
  spaces 
  expr <- 
        parseApplication 
    <|> parseLambda
    <|> parseTerm 
    <|> parseDollarExpr
    <|> parseBracketExpr 
  spaces
  return expr

data LambdaCmd 
  = Run LambdaExpr 
  | Define String LambdaExpr 
  | ChurchNumeral LambdaExpr 
  | ChurchBool LambdaExpr
  | Error String 
  | Quit 
  | Blank 
  | LoadFile String
  | PrintString LambdaExpr

stringEncoding :: String
stringEncoding = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\n!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

parseDecl :: Parser Char LambdaCmd
parseDecl = do
  spaces 
  (T name) <- parseTerm
  spaces
  match ':'
  match '='
  spaces
  Define name <$> parseExpr

parseComment :: Parser Char LambdaCmd
parseComment = do
  spaces
  match '-'
  match '-'
  many $ satisfy (/= '\n')
  return Blank

parseCmd :: Parser Char LambdaCmd
parseCmd = do
  spaces
  match ':'
  (T cmd) <- parseTerm
  spaces
  mexpr <- try parseExpr
  return $ case (map toLower cmd, mexpr) of
    ("cn", Just expr) -> ChurchNumeral expr
    ("churchnumberal", Just expr) -> ChurchNumeral expr
    ("cb", Just expr) -> ChurchBool expr
    ("churchbool", Just expr) -> ChurchBool expr
    ("q", Nothing) -> Quit
    ("l", Just (T name)) -> LoadFile name
    ("ps", Just expr) -> PrintString expr
    _ -> Error $ "Unknown Command '" ++ cmd ++ "'."

parseRoot :: Parser Char LambdaCmd
parseRoot 
    = parseCmd 
  <|> parseComment
  <|> parseDecl 
  <|> (parseExpr >>= \e -> return $ Run e) 
  <|> (spaces >> return Blank)


iterateM :: (Monad m) => a -> (a -> m a) -> m b
iterateM x f = g x
  where g x = f x >>= g

countChurchNumeral :: LambdaExpr -> Maybe Int 
countChurchNumeral (A (T ".inc") e) = (+1) <$> countChurchNumeral e
countChurchNumeral (T ".zero") = Just 0
countChurchNumeral _ = Nothing

countChurchNumeral' :: LambdaExpr -> Maybe Int 
countChurchNumeral' (A (T "__S_inc") e) = (+1) <$> countChurchNumeral' e
countChurchNumeral' (T "__S_zero") = Just 0
countChurchNumeral' _ = Nothing

churchToString :: LambdaExpr -> Maybe String
churchToString (A (A (T "__S_cons") n) ls) = do
  charindex <- countChurchNumeral' n
  rest <- churchToString ls
  return $ (stringEncoding !! charindex):rest
--   return $ 'a':rest

churchToString (T "__S_nil") = Just []
churchToString _ = Nothing


fromChurchBool :: LambdaExpr -> Maybe Bool
fromChurchBool (T ".true") = Just True
fromChurchBool (T ".false") = Just False
fromChurchBool _ = Nothing

main :: IO ()
main = iterateM Map.empty $ \ env -> do
  putStr "> "
  hFlush stdout
  str <- getLine
  runCmd env str

runCmd :: VEnv -> String -> IO VEnv
runCmd env str =
  case parse parseRoot str of
    Just (cmd, "") -> case cmd of
      Define s l -> return $ Map.insert s (evaluate l env)  env
      Run l -> do 
        putStrLn ( showLambda $ evaluate l env) 
        return env
      ChurchNumeral l -> do
        putStrLn $ 
          maybe "Not a Church Numeral" show $ 
            countChurchNumeral $ evaluate (A (A l (T ".inc")) (T ".zero")) env 
        return env
      ChurchBool l -> do
        putStrLn $ 
          maybe "Not a Church Boolean" show $ 
            fromChurchBool $ evaluate (A (A l (T ".true")) (T ".false")) env 
        return env
      PrintString l -> do
        putStrLn $ fromMaybe "Not a String" $
          churchToString (evaluate l env)
        return env
      Error s -> do 
        putStrLn $ "Error: " ++ s
        return env
      Quit -> exitSuccess
      Blank -> return env
      LoadFile s -> do
        f <- openFile (s ++ ".lc") ReadMode
        c <- hGetContents f
        foldM runCmd env (lines c)
    _ -> putStrLn "Parse Error" >> return env



