module Data.StringTrie where

import Codec.Binary.UTF8.String (decode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, unpack)
import Data.String (IsString (fromString))
import Data.Trie (Trie)
import qualified Data.Trie as T

type StringTrie a = Trie a

empty :: StringTrie a
empty = T.empty

keys :: StringTrie a -> [String]
keys = map toString . T.keys

fromList :: [(String, a)] -> StringTrie a
fromList = T.fromList . map (first fromString)

toList :: StringTrie a -> [(String, a)]
toList = map (first toString) . T.toList

elems :: StringTrie a -> [a]
elems = T.elems

submap :: String -> StringTrie a -> StringTrie a
submap = T.submap . fromString

insert :: String -> a -> StringTrie a -> StringTrie a
insert = T.insert . fromString

toString :: ByteString -> String
toString = decode . unpack
