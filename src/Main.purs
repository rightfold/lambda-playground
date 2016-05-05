module Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Function (Fn5, runFn5)
import Data.Lambda (alphaConvert, betaReduce, Term(..))
import Data.Set as Set
import DOM (DOM)
import Prelude

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = runFn5 main' (annotate t) alphaConvert betaReduce id render
  where t = App (App u'' u'') (App y (App (Var unit "f") (Var unit "x")))

        s = Abs "x" (Abs "y" (Abs "z" (App (App (Var unit "x") (Var unit "z"))
                                           (App (Var unit "y") (Var unit "z")))))
        k = Abs "x" (Abs "y" (Var unit "x"))

        u = Abs "f" (App (App (Var unit "f") s) k)

        s' = App u (App u (App u (App u u)))
        k' = App u (App u (App u u))
        u' = Abs "f" (App (App (Var unit "f") s') k')

        s'' = App u' (App u' (App u' (App u' u')))
        k'' = App u' (App u' (App u' u'))
        u'' = Abs "f" (App (App (Var unit "f") s'') k'')

        y  = Abs "f" (App y' y')
        y' = Abs "x" (App (Var unit "f") (App (Var unit "x") (Var unit "x")))

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
  :: Fn5 (Term Ann)
         (Term Ann -> Term Ann)
         (Term Ann -> Term Ann)
         (Term Ann -> Term Ann)
         (Term Ann -> Node)
         (forall eff. Eff (dom :: DOM | eff) Unit)
