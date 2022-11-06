module Data.Term where

import Control.Applicative ((<|>))
import Data.Function (fix)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.StringTrie (StringTrie)
import qualified Data.StringTrie as Trie

data Term
  = Var Int String
  | App Term Term
  | Abs String Term

isVar :: Term -> Bool
isVar (Var _ _) = True
isVar _ = False

isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

newtype ResolveError = NotInScope String

resolve :: StringTrie Term -> Term -> Either ResolveError Term
resolve st (Var _ x) = case Trie.lookup x st of
  Nothing -> Left (NotInScope x)
  Just tm -> Right tm
resolve st (App f x) = App <$> resolve st f <*> resolve st x
resolve st (Abs x t) =
  Abs x <$> resolve (Trie.insert x (Var 0 x) $ fmap (shift 1) st) t

type SmallStep = Term -> Maybe Term

type NanoStep = SmallStep -> SmallStep

bigStep :: SmallStep -> Term -> Term
bigStep step = NonEmpty.last . stepLog step

stepLog :: SmallStep -> Term -> NonEmpty Term
stepLog step = NonEmpty.unfoldr $ (,) <*> step

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

checkCollision :: String -> Term -> Bool
checkCollision s (Var _ _) = False
checkCollision s (Abs x t) = s == x || checkCollision s t
checkCollision s (App f x) = checkCollision s f || checkCollision s x

formatVar :: HashSet String -> Int -> String -> String
formatVar s i x
  | x `HashSet.member` s = x ++ "_" ++ show i
  | otherwise = x

inBracesIf :: String -> Bool -> String
inBracesIf s p
  | p = "(" ++ s ++ ")"
  | otherwise = s

printTerm :: Int -> HashSet String -> Term -> String
printTerm d s (Var i x) = formatVar s (d - i - 1) x
printTerm d s (Abs x t) = "\\" ++ varName ++ " -> " ++ termBody
  where
    mustInsert = not (x `HashSet.member` s) && checkCollision x t
    s' = if mustInsert then HashSet.insert x s else s
    varName = formatVar s' d x
    termBody = printTerm (d + 1) s' t
printTerm d s (App f x) = leftOp ++ " " ++ rightOp
  where
    leftOp = printTerm d s f `inBracesIf` isAbs f
    rightOp = printTerm d s x `inBracesIf` not (isVar x)

instance Show Term where
  show = printTerm 0 HashSet.empty
