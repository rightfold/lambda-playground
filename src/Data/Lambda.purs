module Data.Lambda
( Term(..)
) where

import Prelude

data Term a
  = Var a String
  | Abs String (Term a)
  | App (Term a) (Term a)

instance functorTerm :: Functor Term where
  map f (Var a n) = Var (f a) n
  map f (Abs p b) = Abs p (map f b)
  map f (App c a) = App (map f c) (map f a)
