{-# LANGUAGE LambdaCase #-}

module Command where

import Data.Char (isSpace, isUpper)
import Term

data Command
  = Bind String Term
  | ShowBindings
  | Eval EvalMode Term
  | Load LoadMode [FilePath]
  | Reload
  | Say String
  | Quit

data EvalMode = Trace | Silent

data LoadMode = Reset | Append

commandPrefix :: Char
commandPrefix = ':'

defaultCommand :: Command
defaultCommand =
  Say
    "there is no last command to perform\n\
    \use :? for help."

parseCommand :: String -> Maybe Command
parseCommand (':' : s) | all isSpace s = Nothing
parseCommand s = Just $ case words s of
  ":help" : _ -> Say help
  ":h" : _ -> Say help
  ":?" : _ -> Say help
  ":module" : ms -> parseLoad ms
  ":m" : ms -> parseLoad ms
  ":quit" : _ -> Quit
  ":q" : _ -> Quit
  ":reload" : _ -> Reload
  ":r" : _ -> Reload
  ":trace" : ws -> Eval Trace (error "TODO")
  ":tr" : ws -> Eval Trace (error "TODO")
  ":show" : args -> parseShow args
  ":sh" : args -> parseShow args
  (':' : cmd) : _ -> Say (onUnknown cmd)
  name : "=" : ws -> Bind name (error "TODO")
  ws -> Eval Silent (error "TODO")
  where
    parseLoad ("+" : ms) | areModules ms = Load Append ms
    parseLoad ms | areModules ms = Load Reset ms
    parseLoad _ = Say "syntax:  :module [+] M1 ... Mn"

    areModules = all $ \case
      c : _ -> isUpper c
      _ -> False

    parseShow ("bindings" : _) = ShowBindings
    parseShow _ =
      Say
        "syntax:\
        \    :show bindings"

    onUnknown cmd =
      "unknown command ':" ++ cmd
        ++ "'\n\
           \use :? for help."

    help =
      " Commands available from the prompt:\n\n\
      \   <statement>                 evaluate/run <statement> (TODO)\n\
      \   :                           repeat last command\n\
      \   :{\\n ..lines.. \\n:}\\n       multiline command (TODO)\n\
      \   :help, :?                   display this list of commands\n\
      \   :module [+] <module> ...  \
      \set the context for expression evaluation (TODO)\n\
      \   :quit                       exit Lambda\n\
      \   :reload                     reload the current module set (TODO)\n\n\
      \ -- Commands for debugging:\n\n\
      \   :trace <expr>               evaluate <expr> with tracing on (TODO)\n\n\
      \ -- Commands for displaying information:\n\n\
      \   :show bindings              \
      \show the current bindings made at the prompt (TODO)\n"
