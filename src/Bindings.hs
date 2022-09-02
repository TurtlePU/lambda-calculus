module Bindings where

import Codec.Binary.UTF8.String (decode)
import Data.ByteString (unpack)
import Data.String (IsString (fromString))
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Term

type Bindings = Trie Term

empty = Trie.empty

matchingKeys :: String -> Bindings -> [String]
matchingKeys s = map (decode . unpack) . Trie.keys . Trie.submap (fromString s)
