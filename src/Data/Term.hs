module Data.Term where

import Control.Applicative ((<|>))
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty, last, unfoldr)
import qualified Data.Map as Map
import Data.Map (Map, findWithDefault, keys)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, size)
import qualified Data.Set as Set
import Data.Set (Set, fromList, member)
import Data.StringTrie (StringTrie, insert, lookup)
import Prelude hiding (last, lookup)

data Term
  = Var Int String
  | App Term Term
  | Abs String Term

newtype ResolveError = NotInScope String

resolve :: StringTrie Term -> Term -> Either ResolveError Term
resolve st (Var _ x) = case lookup x st of
  Nothing -> Left (NotInScope x)
  Just tm -> Right tm
resolve st (App f x) = App <$> resolve st f <*> resolve st x
resolve st (Abs x t) =
  Abs x <$> resolve (insert x (Var 0 x) $ fmap (shift 1) st) t

type SmallStep = Term -> Maybe Term

type NanoStep = SmallStep -> SmallStep

bigStep :: SmallStep -> Term -> Term
bigStep step = last . stepLog step

stepLog :: SmallStep -> Term -> NonEmpty Term
stepLog step = unfoldr $ (,) <*> step

smallStep :: NanoStep -> SmallStep
smallStep = fix

normal :: NanoStep
normal _ (App (Abs _ t) x) = Just $ substitute x t
normal step (App f x) = (`App` x) <$> step f <|> App f <$> step x
normal _ _ = Nothing

forced :: NanoStep
forced step (Abs x t) = Abs x <$> step t
forced step t = normal step t

substitute :: Term -> Term -> Term
substitute x = shift (-1) . replace (0, shift 1 x)
  where
    replace :: (Int, Term) -> Term -> Term
    replace (x, t) v@(Var y n) = if x == y then t else v
    replace p (App f x) = App (replace p f) (replace p x)
    replace (x, t) (Abs n v) = Abs n (replace (x + 1, shift 1 t) v)

shift :: Int -> Term -> Term
shift = shift' 0
  where
    shift' b a v@(Var x n) = if x < b then v else Var (x + a) n
    shift' b a (App f x) = App (shift' b a f) (shift' b a x)
    shift' b a (Abs n t) = Abs n (shift' (b + 1) a t)

instance Show ResolveError where
  show (NotInScope x) = "Variable not in scope: " ++ x

checkCollisionsHelper :: Map String Int -> Term -> Map String Int
checkCollisionsHelper m (Var _ _) = m
checkCollisionsHelper m (Abs x t) = let xs = findWithDefault 0 x m in checkCollisionsHelper (Map.insert x (xs + 1) m) t
checkCollisionsHelper m (App f x) = checkCollisionsHelper (checkCollisionsHelper m f) x

checkCollisions :: Term -> Set String
checkCollisions t = fromList $ keys $ Map.filter (== 1) (checkCollisionsHelper Map.empty t)

printTerm :: Set String -> Map String (IntMap Int) -> Int -> Term -> String
printTerm s m d (Var i x) = x ++ if x `member` s then "" else "_" ++ show ((m Map.! x) IntMap.! (d - i))
printTerm s m d (Abs x t) = let xs = findWithDefault (IntMap.empty) x m 
  in "\\" ++ (x ++ if x `member` s then "" else "_" ++ show (size xs)) ++ " -> " ++ printTerm s (Map.insert x (IntMap.insert (d + 1) (size xs) xs) m) (d + 1) t
printTerm s m d (App f x) = leftOp ++ " " ++ rightOp
  where
    leftOp = let showF = printTerm s m d f 
      in case f of
        Abs _ _ -> "(" ++ showF ++ ")"
        _ -> showF
    rightOp = let showX = printTerm s m d x
      in case x of
        Var _ _ -> showX
        _ -> "(" ++ showX ++ ")"

instance Show Term where
  show t = printTerm (checkCollisions t) Map.empty 0 t
