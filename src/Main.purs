module Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Function (Fn5, runFn5)
import Data.Lambda (alphaConvert, betaReduce, Term(..))
import DOM (DOM)
import Prelude

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = runFn5 main' t alphaConvert betaReduce id render
  where t  = App y (Var unit "free")
        y  = Abs "f" (App y' y')
        y' = Abs "x" (App (Var unit "f") (App (Var unit "x") (Var unit "x")))

render :: forall a. Term a -> String
render (Var _ n) = n
render (Abs p b) = "λ" <> p <> "." <> render b
render (App c a) = left c <> " " <> right a
  where left t@(Abs _ _) = "(" <> render t <> ")"
        left t           = render t

        right t@(App _ _) = "(" <> render t <> ")"
        right t           = render t

foreign import main'
  :: Fn5 (Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> Term Unit)
         (Term Unit -> String)
         (forall eff. Eff (dom :: DOM | eff) Unit)
