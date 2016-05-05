module Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Either (Either)
import Data.Function (Fn6, runFn6)
import Data.Lambda (alphaConvert, betaReduce, Term(..))
import Data.Lambda.Parse (parseTerm)
import Data.Set as Set
import DOM (DOM)
import Prelude
import Text.Parsing.Parser (ParseError)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = runFn6 main'
              (annotate t)
              (\s -> parseTerm s <#> annotate)
              alphaConvert
              betaReduce
              id
              render
  where t = App (App cP (cN 10)) (cN 10)

        cN 0 = cZ
        cN n = App cS (cN (n - 1))

        cZ = Abs "f" (Abs "x" (v "x"))
        cS = Abs "n" (Abs "f" (Abs "x" (App (v "f") (App (App (v "n") (v "f")) (v "x")))))

        cP = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (v "m") (v "f")) (App (App (v "n") (v "f")) (v "x"))))))

        v = Var unit

type Ann = {free :: Boolean}

annotate :: forall a. Term a -> Term Ann
annotate = go Set.empty
  where go s (Var _ n) = Var {free: not (Set.member n s)} n
        go s (Abs p b) = Abs p (go (Set.insert p s) b)
        go s (App c a) = App (go s c) (go s a)

render :: Term Ann -> Node
render (Var a n) = varNode a.free n
render (Abs p b) = absNode p (render b)
render (App c a) = appNode (render c) (render a)

foreign import data Node :: *
foreign import varNode :: Boolean -> String -> Node
foreign import absNode :: String -> Node -> Node
foreign import appNode :: Node -> Node -> Node

foreign import main'
  :: Fn6 (Term Ann)
         (String -> Either ParseError (Term Ann))
         (Term Ann -> Term Ann)
         (Term Ann -> Term Ann)
         (Term Ann -> Term Ann)
         (Term Ann -> Node)
         (forall eff. Eff (dom :: DOM | eff) Unit)
