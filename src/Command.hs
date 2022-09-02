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

parseCommand :: String -> Command
parseCommand _ = Help
