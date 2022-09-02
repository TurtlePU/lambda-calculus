{-# LANGUAGE LambdaCase #-}

module Command where

import Data.Char (isSpace, isUpper)
import Term

data Command
  = CBind String Term
  | ShowBindings
  | Eval EvalMode Term
  | Load LoadMode [FilePath]
  | Reload
  | Say Message
  | Quit

data EvalMode = Trace | Silent

data LoadMode = Reset | Append

data Message
  = NoLastCommand
  | ModuleSyntax
  | ShowSyntax
  | UnknownCommand String
  | Help

commandPrefix :: Char
commandPrefix = ':'

parseCommand :: String -> Maybe Command
parseCommand (':' : s) | all isSpace s = Nothing
parseCommand s = Just $ case words s of
  ":help" : _ -> Say Help
  ":h" : _ -> Say Help
  ":?" : _ -> Say Help
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
  (':' : cmd) : _ -> Say (UnknownCommand cmd)
  name : "=" : ws -> CBind name (error "TODO")
  ws -> Eval Silent (error "TODO")
  where
    parseLoad ("+" : ms) | areModules ms = Load Append ms
    parseLoad ms | areModules ms = Load Reset ms
    parseLoad _ = Say ModuleSyntax

    areModules = all $ \case
      c : _ -> isUpper c
      _ -> False

    parseShow ("bindings" : _) = ShowBindings
    parseShow _ = Say ShowSyntax

instance Show Message where
  show NoLastCommand = "there is no last command to perform\nuse :? for help."
  show ModuleSyntax = "syntax:  :module [+] M1 ... Mn"
  show ShowSyntax = "syntax:\n    :show bindings"
  show (UnknownCommand cmd) =
    "unknown command ':" ++ cmd ++ "'\nuse :? for help."
  show Help =
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
    \show the current bindings made at the prompt\n"
