{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Binary.UTF8.String (decode)
import Control.Monad.State.Strict
import Data.ByteString (unpack)
import Data.String (IsString (fromString))
import Data.Trie
import System.Console.Haskeline

main :: IO ()
main = evalStateT (runInputT settings loop) empty
  where
    settings =
      Settings
        { complete = completeFromBindings,
          historyFile = Just ".lambda_history",
          autoAddHistory = False
        }

completeFromBindings :: CompletionFunc (StateT Bindings IO)
completeFromBindings = completeWord escapeChar whitespace impl
  where
    escapeChar = Nothing
    whitespace = " ()\\>"
    impl s = map simpleCompletion . matchingKeys s <$> get

loop :: InputT (StateT Bindings IO) ()
loop = return ()

data Term

type Bindings = Trie Term

matchingKeys :: String -> Bindings -> [String]
matchingKeys s = map (decode . unpack) . keys . submap (fromString s)
