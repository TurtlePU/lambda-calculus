module Command where

import Term

data Command
  = Bind String Term
  | ShowBindings
  | Eval EvalMode Term
  | Load LoadMode [FilePath]
  | Reload
  | SetPrompt String
  | Repeat
  | Help
  | Quit

data EvalMode = Trace | Silent

data LoadMode = Reset | Append

commandPrefix :: Char
commandPrefix = ':'

onRepeat :: String
onRepeat =
  "there is no last command to perform\n\
  \use :? for help."

helpText :: String
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
  \   :trace <expr>               evaluate <expr> with tracing on (TODO)\n\n\
  \ -- Commands for changing settings:\n\n\
  \   :set prompt <prompt>        set the prompt used in Lambda (TODO)\n\n\
  \ -- Commands for displaying information:\n\n\
  \   :show bindings              \
  \show the current bindings made at the prompt (TODO)\n"

parseCommand :: String -> Command
parseCommand _ = Help
