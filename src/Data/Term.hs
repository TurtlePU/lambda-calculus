module Data.Term where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty, last, unfoldr)
import Prelude hiding (last)

data Term
  = Var String Int
  | App Term Term
  | Abs String Term

normalOrder :: Term -> Term
normalOrder = last . normalOrderLog

normalOrderLog :: Term -> NonEmpty Term
normalOrderLog = unfoldr $ (,) <*> normalOrderStep

normalOrderStep :: Term -> Maybe Term
normalOrderStep (App (Abs _ t) x) = Just $ substitute x t
normalOrderStep (App f x) =
  (`App` x) <$> normalOrderStep f
    <|> App f <$> normalOrderStep x
normalOrderStep _ = Nothing

substitute :: Term -> Term -> Term
substitute x = shift (-1) . replace (0, shift 1 x)
  where
    shift = shift' 0

    shift' :: Int -> Int -> Term -> Term
    shift' b a v@(Var n x) = if x < b then v else Var n (x + a)
    shift' b a (App f x) = App (shift' b a f) (shift' b a x)
    shift' b a (Abs n t) = Abs n (shift' (b + 1) a t)

    replace :: (Int, Term) -> Term -> Term
    replace (x, t) v@(Var n y) = if x == y then t else v
    replace p (App f x) = App (replace p f) (replace p x)
    replace (x, t) (Abs n v) = Abs n (replace (x + 1, shift 1 t) v)

instance Show Term where
  show (Var x _) = x
  show (App f x@(Var _ _)) = show f ++ " " ++ show x
  show (App f x) = show f ++ " (" ++ show x ++ ")"
  show (Abs x t) = "\\" ++ x ++ " -> " ++ show t
