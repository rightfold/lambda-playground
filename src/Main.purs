module Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Function (Fn5, runFn5)
import Data.Lambda (alphaConvert, Term(..))
import DOM (DOM)
import Prelude

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = runFn5 main' y alphaConvert id id render
  where y  = Abs "f" (App y' y')
        y' = Abs "x" (App (Var unit "f") (App (Var unit "x") (Var unit "x")))

render :: forall a. Term a -> String
render (Var _ n) = n
render (Abs p b) = "λ" <> p <> "." <> render b
render (App c a) =
  case c of
    Abs _ _ -> "(" <> render c <> ") " <> render a
    _       -> render c <> " " <> render a

foreign import main'
  :: Fn5 (Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> String)
         (forall eff. Eff (dom :: DOM | eff) Unit)
