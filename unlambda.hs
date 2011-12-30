import System.IO
import System
import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Control.Exception (catchJust)
import System.IO.Error

type EvalState = StateT (Maybe Char) IO

data Term = I | K | K2 Term | S | S2 Term | S3 Term Term | V | E 
          | Readchar | Printchar Char | Compchar Char | Reprint | D | D2 Term
          | C | Callcc Cont | App Term Term 
            deriving (Eq, Show)

data Cont = Exiter | Dcheck Term Cont | D_delayed Term Cont 
          | D_undelayed Term Cont
              deriving (Eq, Show)

catchEOF = catchJust (guard.isEOFError)

maybeChar :: IO (Maybe Char)
maybeChar = fmap Just getChar `catchEOF` (\_ -> return Nothing)

app :: Term -> Term -> Cont -> EvalState (Cont, Term)
app I a c = return (c,a)
app K a c = return (c,K2 a)
app (K2 a) _ c = return (c,a)
app S a c = return (c,S2 a)
app (S2 a) a2 c = return (c,S3 a a2)
app (S3 a a2) a3 c = descend c (App (App a a3) (App a2 a3))
app V _ c = return (c,V)
app E a c = liftIO $ exitWith (if a == I then ExitSuccess else ExitFailure 1)
app (D2 right) a c = descend (D_delayed a c) right
app Readchar a c = do
  curchar <- liftIO maybeChar
  put curchar
  descend c (App a $ maybe V (const I) curchar)
app (Printchar char) a c = liftIO (putChar char) >> return (c,a)
app (Compchar char) a c = do
  cchar <- get
  let eq = fromMaybe False ((==) <$> cchar <*> Just char)
  descend c (App a (if eq then  I else V))
app Reprint a c = do
  cchar <- get
  descend c (App a $ maybe V Printchar cchar)
app C a c = descend c (App a $ Callcc c)
app (Callcc cont) a _ = return (cont, a)
-- These things are never actually applied:
app D _ _ = error "D: this never happens"
app (App _ _) _ _ = error "App: this never happens"

eval :: Cont -> Term -> EvalState (Cont, Term)
eval Exiter a = app E a Exiter
eval (D_delayed rv k2) lv = app lv rv k2
eval (D_undelayed lv cont) rv = app lv rv cont
eval (Dcheck right cont) D = eval cont (D2 right)
eval (Dcheck right cont) lv = descend (D_undelayed lv cont) right

descend :: Cont -> Term -> EvalState (Cont, Term)
descend cont (App left right) = descend (Dcheck right cont) left
descend cont tree = eval cont tree

run :: Term -> IO ()
run tree = run' start Nothing
    where 
      start = descend Exiter tree
      e' = uncurry eval
      run' start state = runStateT start state >>= \(r,n) -> run' (e' r) n

hBuild :: Handle -> IO Term
hBuild h = do 
  c <- hGetChar h
  case c of 
    '#' -> hGetLine h >> hBuild h
    '`' -> do
            left <- hBuild h
            right <- hBuild h
            return $ App left right
    _ | takesone c -> return $ buildone c
      | takestwo c ->  do
            arg <- hGetChar h
            return $ buildtwo c arg
      | otherwise -> hBuild h

build :: String -> Maybe Term
build = maybe Nothing (Just . fst) . build'
    where
      build' :: String -> Maybe (Term, String)
      build' [] = Nothing
      build' (c:cs)
          | c == '`' = do
              (left, cs') <- build' cs
              (right, rest) <- build' cs'
              return ((App left right), rest)
          | c == '#' = case snd $ break (=='\n') cs of
                         "" -> Nothing
                         rst -> build' $ tail rst
          | takesone c = Just (buildone c, cs)
          | takestwo c = case cs of
                           (arg:rest) -> Just (buildtwo c arg, rest)
                           [] -> Nothing
          | otherwise = build' cs

buildone 'i' = I
buildone 'v' = V
buildone 'c' = C
buildone 'e' = E
buildone 'd' = D
buildone 's' = S
buildone 'k' = K
buildone '|' = Reprint
buildone 'r' = Printchar '\n'
buildone '@' = Readchar
buildtwo '.' c = Printchar c
buildtwo '?' c = Compchar c
takesone = flip elem "ivcedsk|r@"
takestwo = flip elem ".?"

main = do
  args <- getArgs
  handle <- if not$null args then openFile (head args) ReadMode else return stdin
  hSetEncoding handle latin1
  tree <- fmap Just (hBuild handle) `catchEOF`
          (\_ -> putStrLn "Error: input too short" >> return Nothing)
  case tree of 
    Just t -> run t
    Nothing -> return ()
