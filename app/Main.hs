module Main where

import Command
import Control.Monad.State.Strict
import Data.Functor (($>))
import Data.Labeled
import Data.StringTrie
import System.Console.Haskeline
import Term (Term)

----------------------------------- AppState -----------------------------------

data AppState = App {bindings :: StringTrie Term, lastCommand :: Command}

matchingKeys :: String -> AppState -> [String]
matchingKeys s = keys . submap s . bindings

writeCmd :: Command -> AppState -> AppState
writeCmd c s = s {lastCommand = c}

allBindings :: AppState -> [Labeled Term]
allBindings = map (uncurry Label) . toList . bindings

------------------------------------- REPL -------------------------------------

main :: IO ()
main = evalStateT (runInputT settings loop) app
  where
    app = App {bindings = empty, lastCommand = Say NoLastCommand}
    settings =
      Settings
        { complete = completeFromBindings,
          historyFile = Just ".lambda_history",
          autoAddHistory = True
        }

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
    (Bind l) -> reply "TODO"
    (Eval em te) -> reply "TODO"
    ShowBindings -> do
      bindings <- lift $ allBindings <$> get
      reply . unlines $ map show bindings
    (Load lm ss) -> reply "TODO"
    Reload -> reply "TODO"
    Say msg -> reply $ show msg
    Quit -> outputStrLn "Leaving Lambda."
  where
    reply str = outputStrLn str >> loop
