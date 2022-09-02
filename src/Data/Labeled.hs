module Data.Labeled where

data Labeled a = Label String a

instance Show a => Show (Labeled a) where
  show (Label name x) = name ++ " = " ++ show x
