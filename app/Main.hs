{-# LANGUAGE FlexibleContexts #-}

module Main where

import Bindings
import Control.Monad.State.Strict
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
