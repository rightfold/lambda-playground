module Data.Lambda.Parse
( parseTerm
) where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Lazy (defer)
import Control.Monad.Trampoline (runTrampoline, Trampoline)
import Data.Array ((..))
import Data.Char as Char
import Data.Either (Either(Right))
import Data.Foldable (foldl)
import Data.Lambda (Term(..))
import Data.List (some)
import Data.List as List
import Data.List.Unsafe as Unsafe
import Data.String as String
import Prelude
import Text.Parsing.Parser (ParserT, PState(PState), runParserT)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser.String (char, eof, oneOf, whiteSpace)

parseTerm :: String -> Term Unit
parseTerm s = case runTrampoline $ runParserT (PState {input: s, position: initialPos}) (whiteSpace *> defer term <* eof) of
                Right t -> t

type P a = ParserT String Trampoline a

lxm :: forall a. P a -> P a
lxm p = p <* whiteSpace

lambda :: P Unit
lambda = void $ lxm (oneOf ['\\', 'λ'])

period :: P Unit
period = void $ lxm (char '.')

lparen :: P Unit
lparen = void $ lxm (char '(')

rparen :: P Unit
rparen = void $ lxm (char ')')

identifier :: P String
identifier = lxm ((String.fromCharArray <<< List.toUnfoldable) <$> some (oneOf letters))
  where letters = map Char.fromCharCode (97 .. 122)

integer :: P Int
integer = lxm ((parseInt <<< String.fromCharArray <<< List.toUnfoldable) <$> some (oneOf digits))
  where digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
foreign import parseInt :: String -> Int

term :: Unit -> P (Term Unit)
term _ = defer abs

abs :: Unit -> P (Term Unit)
abs _ = this <|> next
  where this = do
          lambda
          name <- identifier
          period
          body <- defer abs
          pure $ Abs name body
        next = defer app

app :: Unit -> P (Term Unit)
app _ = foldl1 App <$> some next
  where foldl1 f xs = foldl f (Unsafe.head xs) (Unsafe.tail xs)
        next = defer var

var :: Unit -> P (Term Unit)
var _ = this <|> natural <|> parend
  where this = Var unit <$> identifier
        natural = (Abs "f" <<< Abs "x" <<< go) <$> integer
          where go 0 = Var unit "x"
                go n = App (Var unit "f") (go (n - 1))
        parend = lparen *> defer term <* rparen
