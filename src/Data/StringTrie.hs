module Data.StringTrie where

import Codec.Binary.UTF8.String (decode)
import Data.ByteString (unpack)
import Data.String (IsString (fromString))
import Data.Trie (Trie)
import qualified Data.Trie as T

type StringTrie a = Trie a

empty :: StringTrie a
empty = T.empty

keys :: StringTrie a -> [String]
keys = map (decode . unpack) . T.keys

submap :: String -> StringTrie a -> StringTrie a
submap = T.submap . fromString
