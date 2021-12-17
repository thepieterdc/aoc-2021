module Utils.Parser(module Utils.Parser, module X) where

import Control.Applicative as X
import Control.Monad as X

-- |The type for Parsers.
newtype Parser a = Parser (String -> [(a, String)])

doParse :: (Show a) => Parser a -> String -> a
doParse m s = one [x | (x,t) <- apply m s, t == "" ] where
  one [x] = x
  one [] = error ("Parse not completed:\n " ++ show s) 
  one xs | length xs > 1 = error ("Multiple parses found:\n " ++ show xs)
  one _ = error "Unknown parse error."

-- |Applies a parser.
apply :: Parser a -- ^ The parser to apply
      -> String -- ^ The string to apply the parser on
      -> [(a, String)] -- ^ The parsed expression and the remaining string
apply (Parser f) = f

-- |The functor instance for a Parser.
instance Functor Parser where
  fmap = liftM

-- |The Applicative instance for a Parser.
instance Applicative Parser where
  pure = return
  (<*>) = ap

-- |The Monad instance for a Parser.
instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  m >>= k = Parser (\s -> [(y, u) |
                           (x, t) <- apply m s,
                           (y, u) <- apply (k x) t])

-- |The Alternative instance for a Parser.
instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser (\s ->
                      case apply p s of
                        [] -> apply q s
                        res -> res)
  some p = do { x <- p; xs <- many p; return (x:xs)}
  many p = some p `mplus` return []

-- |The MonadPlus instance for a Parser.
instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus m n = Parser (ap ((++) . apply m) (apply n))

char :: Parser Char
char = Parser f where
  f [] = []
  f (c:s) = [(c,s)]

chars :: Int -> Parser String
chars 0 = return ""
chars amt = do { x <- char; y <- chars (amt - 1); return (x : y)}

spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c}

string :: String -> Parser String
string = mapM (spot . (==))

token :: Char -> Parser ()
token c = void (spot (c == ))