module Command where

import Data.Labeled (Labeled)
import Data.Term (Term)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

data Command
  = Bind (Labeled Term)
  | ShowBindings
  | Eval Normalization Tracing Term
  | Load LoadMode [FilePath]
  | Reload
  | Say Message
  | Quit

data Normalization = Normal | Forced

data Tracing = Trace | Silent

data LoadMode = Reset | Append

----------------------------------- Messages -----------------------------------

type ParsecError = ParseErrorBundle String Void

data Message
  = NoLastCommand
  | ShowSyntax
  | UnknownCommand String
  | ParseError ParsecError
  | Help

instance Show Message where
  show NoLastCommand = "there is no last command to perform\nuse :? for help."
  show ShowSyntax = "syntax:\n    :show bindings"
  show (UnknownCommand cmd) =
    "unknown command ':" ++ cmd ++ "'\nuse :? for help."
  show (ParseError err) = errorBundlePretty err
  show Help =
    " Commands available from the prompt:\n\n\
    \   <statement>                 evaluate/run <statement>\n\
    \   :                           repeat last command\n\
    \   :{\\n ..lines.. \\n:}\\n       multiline command (TODO)\n\
    \   :help, :?                   display this list of commands\n\
    \   :module [+] <module> ...  \
    \set the context for expression evaluation (TODO)\n\
    \   :quit                       exit Lambda\n\
    \   :reload                     reload the current module set (TODO)\n\n\
    \ -- Commands for debugging:\n\n\
    \   :trace <expr>               evaluate <expr> with tracing on\n\
    \   :force [trace] <expr>       force complete <expr> evaluation\n\n\
    \ -- Commands for displaying information:\n\n\
    \   :show bindings              \
    \show the current bindings made at the prompt\n"
