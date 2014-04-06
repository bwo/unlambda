import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Control.Exception (catchJust, throwIO)
import GHC.IO.Exception
import System.IO.Error
import Control.Monad.MaybeT

infixl 3 ¢
infixl 4 $>
type EvalState = StateT (Maybe Char) IO

data Term = I | K | K2 Term | S | S2 Term | S3 Term Term | V | E 
          | Readchar | Printchar Char | Compchar Char | Reprint | D | D2 Term
          | C | Callcc Cont | App Term Term 
            deriving (Eq, Show)

data Cont = Exiter | Dcheck Term Cont | D_delayed Term Cont 
          | D_undelayed Term Cont
              deriving (Eq, Show)

catchEOF = catchJust (guard.isEOFError)
eofError = IOError Nothing EOF "" "" Nothing Nothing
hMaybeChar :: Handle -> IO (Maybe Char)
hMaybeChar h = fmap Just (hGetChar h) `catchEOF` (\_ -> return Nothing)
maybeChar = hMaybeChar stdin

($>) :: Applicative f => f (a -> b) -> a -> f b
f $> a = f <*> (pure a)

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
  let eq = fromMaybe False ((==) <$> cchar $> char)
  descend c (App a (if eq then  I else V))
app Reprint a c = get >>= descend c . App a . maybe V Printchar
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

buildM :: (Monad m) => m (Maybe Char) -> m (Maybe Term)
buildM charaction = runMaybeT go
    where
      action = MaybeT charaction
      go = do
        c <- action
        case c of
          '`' -> App <$> go <*> go
          '#' -> line
              where line = action >>= (\n -> if n == '\n' then go else line)
          _ -> lookup c one ¢ return <?> (lookup c two ¢ (`fmap` action) <?> go)
      one = [('i', I), ('v',V), ('c',C), ('e', E), ('d', D), ('s',S), ('k',K),
             ('r', Printchar '\n'), ('@',Readchar), ('|',Reprint)]
      two = [('.', Printchar), ('?',Compchar)]

(¢) = flip ($)
(<?>) = flip maybe

hBuild :: Handle -> IO Term
hBuild h = buildM (hMaybeChar h) >>= maybe (throwIO eofError) return

build :: String -> Maybe Term
build = evalState (buildM listgetchar)
    where
      listgetchar = do
        st <- get
        case st of
          [] -> return Nothing
          (c:cs) -> put cs >> return (Just c)

main = do
  args <- getArgs
  handle <- if not$null args then openFile (head args) ReadMode else return stdin
  hSetEncoding handle latin1
  tree <- fmap Just (hBuild handle) `catchEOF`
          (\_ -> putStrLn "Error: input too short" >> return Nothing)
  when (handle /= stdin) $ hClose handle
  maybe (return ()) run tree
