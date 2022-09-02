{-# LANGUAGE LambdaCase #-}

module Command where

import Data.Char (isSpace)
import Data.Labeled (Labeled (Label))
import Data.List (foldl1')
import Data.StringTrie
import Data.Term (Term (..))
import Data.Void (Void)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Command
  = Bind (Labeled Term)
  | ShowBindings
  | Eval EvalMode Term
  | Load LoadMode [FilePath]
  | Reload
  | Say Message
  | Quit

data EvalMode = Trace | Silent

data LoadMode = Reset | Append

------------------------------------ Parser ------------------------------------

type Parser = Parsec Void String

type ParsecError = ParseErrorBundle String Void

prefix :: Char
prefix = ':'

parseCommand :: String -> Maybe Command
parseCommand s = case runParser command "<interactive>" s of
  Left err -> Just . Say $ ParseError err
  Right ok -> ok
  where
    command =
      char prefix *> explicit
        <|> Just . Bind <$> try binding
        <|> Just . Eval Silent <$> term

    explicit =
      takeWhileP Nothing (not . isSpace) >>= \case
        "" -> pure Nothing
        w -> case elems (submap w finishes) of
          finish : _ -> Just <$> finish
          _ -> return . Just . Say $ UnknownCommand w

    binding = Label <$> lex name <* char '=' <*> term
    term = sc *> (foldl1' App <$> sepEndBy1 atom sc)
    atom =
      char '\\' *> (Abs <$> lex name <* string "->" <*> term)
        <|> between (char '(') (char ')') term
        <|> Var 0 <$> name

    finishes :: StringTrie (Parser Command)
    finishes =
      fromList
        [ ("help", return $ Say Help),
          ("?", return $ Say Help),
          ("module", Load <$> loadMode <* sc <*> sepEndBy name sc),
          ("quit", return Quit),
          ("reload", return Reload),
          ("trace", Eval Trace <$> term),
          ("show", ShowBindings <$ sc <* string "bindings" <|> return (Say ShowSyntax))
        ]
    loadMode = maybe Reset (const Append) <$> optional (sym "+")

    name = (:) <$> letterChar <*> many alphaNumChar
    lex = L.lexeme sc
    sym = L.symbol sc
    sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

----------------------------------- Messages -----------------------------------

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
    \   :trace <expr>               evaluate <expr> with tracing on\n\n\
    \ -- Commands for displaying information:\n\n\
    \   :show bindings              \
    \show the current bindings made at the prompt\n"
