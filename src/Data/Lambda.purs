module Data.Lambda
( Term(..)
, alphaConvert
, betaReduce
) where

import Control.Monad.State (evalState)
import Control.Monad.State.Class (gets, class MonadState, modify)
import Data.Array ((..), (!!), length, filter)
import Data.Char (fromCharCode)
import Data.Char as Char
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Data.String as String
import Prelude

data Term a
  = Var a String
  | Abs String (Term a)
  | App (Term a) (Term a)

instance functorTerm :: Functor Term where
  map f (Var a n) = Var (f a) n
  map f (Abs p b) = Abs p (map f b)
  map f (App c a) = App (map f c) (map f a)

alphaConvert :: forall a. Term a -> Term a
alphaConvert t = evalState (go Map.empty t) 0
  where go :: forall b m. (MonadState Int m) => Map String String -> Term b -> m (Term b)
        go m (Var a n) = pure $ Var a (fromMaybe n (Map.lookup n m))
        go m (Abs p b) = do
          modify (_ + 1)
          p' <- gets letter
          Abs p' <$> go (Map.insert p p' m) b
        go m (App c a) = App <$> go m c <*> go m a

        letter :: Int -> String
        letter n = case letters !! n of
                     Just l  -> l
                     Nothing ->    letter (n -     (length letters))
                                <> letter (n `mod` (length letters))

        letters :: Array String
        letters = filter (not <<< flip String.contains bad) all
          where bad = "λςοаерськ"
                all = do
                  {offset, count} <- ranges
                  map (Char.toString <<< fromCharCode <<< (_ + offset)) (0 .. (count - 1))
                ranges = [ {offset: 0x0061, count: 26}
                         , {offset: 0x03B1, count: 25}
                         , {offset: 0x0430, count: 32}
                         ]


betaReduce :: forall a. Term a -> Term a
betaReduce = alphaConvert >>> go Map.empty >>> alphaConvert
  where go :: forall b. Map String (Term b) -> Term b -> Term b
        go m v@(Var _ n) = fromMaybe v (Map.lookup n m)
        go m   (Abs p b) = Abs p (go m b)
        go m   (App c a) =
          case c of
            (Abs p b) -> go (Map.insert p (go m a) m) b
            _ -> App (go m c) (go m a)
