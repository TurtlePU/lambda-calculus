module Main where

import Command
import Control.Monad.State.Strict
import Data.Functor (($>))
import Data.StringTrie
import System.Console.Haskeline
import Term (Term)

----------------------------------- AppState -----------------------------------

data AppState = App {bindings :: StringTrie Term, lastCommand :: Command}

matchingKeys :: String -> AppState -> [String]
matchingKeys s = keys . submap s . bindings

writeCmd :: Command -> AppState -> AppState
writeCmd c s = s {lastCommand = c}

------------------------------------- REPL -------------------------------------

main :: IO ()
main = evalStateT (runInputT settings loop) app
  where
    settings =
      Settings
        { complete = completeFromBindings,
          historyFile = Just ".lambda_history",
          autoAddHistory = True
        }
    app = App {bindings = empty, lastCommand = defaultCommand}

completeFromBindings :: CompletionFunc (StateT AppState IO)
completeFromBindings = completeWord escapeChar whitespace impl
  where
    impl :: Monad m => String -> StateT AppState m [Completion]
    impl s = map simpleCompletion . matchingKeys s <$> get
    escapeChar = Just commandPrefix
    whitespace = " ()\\>"

loop :: InputT (StateT AppState IO) ()
loop = do
  line <- getInputLine "> "
  command <- lift $ case maybe (Just Quit) parseCommand line of
    Nothing -> lastCommand <$> get
    Just cmd -> modify (writeCmd cmd) $> cmd
  case command of
    (Bind s te) -> say "TODO"
    ShowBindings -> say "TODO"
    (Eval em te) -> say "TODO"
    (Load lm ss) -> say "TODO"
    Reload -> say "TODO"
    Say text -> say text
    Quit -> outputStrLn "Leaving Lambda."
  where
    say str = outputStrLn str >> loop
