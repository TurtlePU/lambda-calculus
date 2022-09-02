{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Command (Command (..), parseCommand)
import Control.Monad (void)
import Control.Monad.State.Strict
import Data.Functor (($>))
import Data.StringTrie
import System.Console.Haskeline
import Term (Term)

----------------------------------- AppState -----------------------------------

data AppState = App {bindings :: StringTrie Term, lastCommand :: Command}

app :: AppState
app = App empty Repeat

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

completeFromBindings :: CompletionFunc (StateT AppState IO)
completeFromBindings = completeWord escapeChar whitespace impl
  where
    escapeChar = Just ':'
    whitespace = " ()\\>"
    impl s = map simpleCompletion . matchingKeys s <$> get

loop :: InputT (StateT AppState IO) ()
loop =
  getInputLine "> " >>= \case
    Nothing -> exit
    Just line -> do
      command <- lift $ case parseCommand line of
        Repeat -> lastCommand <$> get
        cmd -> modify (writeCmd cmd) $> cmd
      case command of
        (Bind s te) -> say "TODO"
        ShowBindings -> say "TODO"
        (Eval em te) -> say "TODO"
        (Load lm ss) -> say "TODO"
        Reload -> say "TODO"
        (SetPrompt s) -> say "TODO"
        Repeat -> say noLastCommand
        Help -> say helpText
        Quit -> exit
  where
    say str = outputStrLn str >> loop
    exit = void $ outputStrLn "Leaving Lambda."
    noLastCommand =
      "there is no last command to perform\n\
      \use :? for help."
    helpText =
      " Commands available from the prompt:\n\n\
      \   <statement>                 evaluate/run <statement> (TODO)\n\
      \   :                           repeat last command (TODO)\n\
      \   :{\\n ..lines.. \\n:}\\n       multiline command (TODO)\n\
      \   :help, :?                   display this list of commands\n\
      \   :module [+] <module> ...  \
      \set the context for expression evaluation (TODO)\n\
      \   :quit                       exit Lambda (TODO)\n\
      \   :reload                     reload the current module set (TODO)\n\n\
      \ -- Commands for debugging:\n\n\
      \   :trace <expr>               \
      \evaluate <expr> with tracing on (TODO)\n\n\
      \ -- Commands for changing settings:\n\n\
      \   :set prompt <prompt>        set the prompt used in Lambda (TODO)\n\n\
      \ -- Commands for displaying information:\n\n\
      \   :show bindings              \
      \show the current bindings made at the prompt (TODO)\n"
