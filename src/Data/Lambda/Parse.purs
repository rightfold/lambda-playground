module Data.Lambda.Parse
( parseTerm
) where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Lazy (defer)
import Control.Monad.Trampoline (runTrampoline, Trampoline)
import Data.Array ((..))
import Data.Char as Char
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Lambda (Term(..))
import Data.List (some)
import Data.List as List
import Data.List.Unsafe as Unsafe
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Prelude
import Text.Parsing.Parser (fail, ParseError, ParserT, PState(PState), runParserT)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser.String (char, eof, oneOf, whiteSpace)

parseTerm :: String -> Either ParseError (Term Unit)
parseTerm s = runTrampoline $ runParserT (PState {input: s, position: initialPos})
                                         (whiteSpace *> defer term <* eof)

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

hash :: P Unit
hash = void $ lxm (char '#')

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
var _ = this <|> natural <|> macro <|> parend
  where this = Var unit <$> identifier
        natural = (Abs "f" <<< Abs "x" <<< go) <$> integer
          where go 0 = Var unit "x"
                go n = App (Var unit "f") (go (n - 1))
        macro = do
          name <- hash *> identifier
          case go name of
            Just t  -> pure t
            Nothing -> fail $ "Unknown macro '" <> name <> "'"
          where go "and"   = Just $ Abs "p" (Abs "q" (App (App p q) p))
                go "or"    = Just $ Abs "p" (Abs "q" (App (App p p) q))
                go "not"   = Just $ not
                go "xor"   = Just $ Abs "p" (Abs "q" (App (App p (App not q)) q))
                go "if"    = Just $ Abs "p" (Abs "x" (Abs "y" (App (App p x) y)))
                go "true"  = Just $ true_
                go "false" = Just $ false_

                go "iszero" = Just $ Abs "n" (App (App n (Abs "x" false_)) true_)

                go "pair"    = Just $ pair
                go "first"   = Just $ first
                go "second"  = Just $ second
                go "curry"   = Just $ Abs "f" (Abs "x" (Abs "y" (App f (App (App pair x) y))))
                go "uncurry" = Just $ Abs "f" (Abs "p" (App (App f (App first p)) (App second p)))

                go _        = Nothing

                true_  = Abs "x" (Abs "y" x)
                false_ = Abs "x" (Abs "y" y)
                not    = Abs "p" (App (App p false_) true_)

                pair   = Abs "x" (Abs "y" (Abs "z" (App (App z x) y)))
                first  = Abs "p" (App p true_)
                second = Abs "p" (App p false_)

                f = Var unit "f"
                n = Var unit "n"
                p = Var unit "p"
                q = Var unit "q"
                x = Var unit "x"
                y = Var unit "y"
                z = Var unit "z"
        parend = lparen *> defer term <* rparen
