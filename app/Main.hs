{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Bindings
import Command
import Control.Monad (void)
import Control.Monad.State.Strict
import System.Console.Haskeline

data AppState = App {bindings :: Bindings, lastCommand :: Command}

writeCmd :: Command -> AppState -> AppState
writeCmd c s = s { lastCommand = c }

main :: IO ()
main = evalStateT (runInputT settings loop) (App empty Repeat)
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
    escapeChar = Nothing
    whitespace = " ()\\>"
    impl s = map simpleCompletion . matchingKeys s . bindings <$> get

loop :: InputT (StateT AppState IO) ()
loop =
  getInputLine "> " >>= \case
    Nothing -> exit
    Just line -> do
      command <- lift $ case parseCommand line of
        Repeat -> lastCommand <$> get
        cmd -> cmd <$ modify (writeCmd cmd)
      result <- case command of
        (Bind s te) -> return (Just "TODO")
        ShowBindings -> return (Just "TODO")
        (Eval em te) -> return (Just "TODO")
        (Load lm ss) -> return (Just "TODO")
        Reload -> return (Just "TODO")
        (SetPrompt s) -> return (Just "TODO")
        Repeat -> return (Just noLastCommand)
        Help -> return (Just helpText)
        Quit -> return Nothing
      case result of
        Just str -> outputStrLn str >> loop
        Nothing -> exit
  where
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
