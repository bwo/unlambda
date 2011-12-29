{-# LANGUAGE ScopedTypeVariables #-}
import System.IO
import Prelude hiding (catch)
import System
import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Control.Exception (catchJust)
import System.IO.Error
import Debug.Trace

type EvalState = StateT (Maybe Char) IO

data Term = I | K | K2 Term | S | S2 Term | S3 Term Term | V | E 
          | Readchar | Printchar Char | Compchar Char | Reprint | D | D2 Term
          | C | Callcc Cont | App Term Term | Pair Cont Term 
            deriving (Eq, Show)

data Cont = Descend | Exiter | Dcheck Term Cont |
          D_delayed Term Cont | D_undelayed Term Cont
              deriving (Eq, Show)

maybeChar :: IO (Maybe Char)
maybeChar = catchJust (guard.isEOFError) (getChar >>= return.Just) (\_ -> return Nothing)

app :: Term -> Term -> Cont -> EvalState (Cont, Term)
app I a c = return (c,a)
app K a c = return (c,K2 a)
app (K2 a) _ c = return (c,a)
app S a c = return (c,S2 a)
app (S2 a) a2 c = return (c,(S3 a a2))
app (S3 a a2) a3 c = return (Descend, Pair c (App (App a a3) (App a2 a3)))
app V _ c = return (c,V)
app E a c = liftIO $ exitWith (if a == I then ExitSuccess else ExitFailure 1)
app (D2 right) a c = return (Descend,Pair (D_delayed a c) right)
app Readchar a c = do
  curchar <- liftIO maybeChar
  put curchar
  return (Descend,Pair c (App a $ maybe V (const I) curchar))
app (Printchar char) a c = do
  liftIO (putChar char)
  return (c,a)
app (Compchar char) a c = do
  cchar <- get
  let eq = maybe False id ((==) <$> cchar <*> Just char)
  return (Descend, Pair c (App a (if eq then I else V)))
app Reprint a c = do
  cchar <- get
  return (Descend, Pair c (App a $ maybe V Printchar cchar))
app C a c = return (Descend, Pair c (App a $ Callcc c))
app (Callcc cont) a _ = return (cont, a)
-- These things are never actually applied:
app D _ _ = error "D: this never happens"
app (App _ _) _ _ = error "App: this never happens"
app (Pair _ _) _ _ = error "Pair: this never happens"

eval :: Cont -> Term -> EvalState (Cont, Term)
eval Descend (Pair cont (App left right)) = return (Descend, Pair dchecker left)
    where dchecker = Dcheck right cont
eval Descend (Pair cont tree) = eval cont tree
eval Descend _ = error "Descend: this never happens"
eval Exiter a = app E a Exiter
eval (D_delayed rv k2) lv = app lv rv k2
eval (D_undelayed lv cont) rv = app lv rv cont
eval (Dcheck right cont) D = eval cont (D2 right)
eval (Dcheck right cont) lv = return (Descend, Pair (D_undelayed lv cont) right)


run tree = run' start Nothing
    where 
      start = eval Descend (Pair Exiter tree)
      e' = uncurry eval
      run' start state = runStateT start state >>= \(r,n) -> run' (e' r) n

build :: Handle -> IO Term
build h = do 
  c <- hGetChar h
  case c of 
    '#' -> hGetLine h >> build h
    '`' -> do
            left <- build h
            right <- build h
            return $ App left right
    _ -> if takesone c
         then return $ buildone c
         else if takestwo c
              then do 
                arg <- hGetChar h
                return $ buildtwo c arg
              else
                  build h
      where
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
  handle <- if length args /= 0 then openFile (args!!0) ReadMode else return stdin
  hSetEncoding handle latin1
  tree <- catchJust (guard.isEOFError) (build handle >>= return. Just) 
          (\_ -> putStrLn "Error: input too short" >> return Nothing)
  case tree of 
    (Just t) -> run t >> return ()
    Nothing -> return ()
