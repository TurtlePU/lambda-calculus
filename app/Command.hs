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
  | Eval Normalization Tracing Term
  | Load LoadMode [FilePath]
  | Reload
  | Say Message
  | Quit

data Normalization = Normal | Forced

data Tracing = Trace | Silent

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
        <|> Just . Bind <$> try binding <* eof
        <|> Just . Eval Normal Silent <$> term <* eof

    explicit =
      takeWhileP Nothing (not . isSpace) >>= \case
        "" -> pure Nothing
        w -> case elems (submap w finishes) of
          finish : _ -> Just <$> finish
          _ -> return . Just . Say $ UnknownCommand w

    binding = packBinding <$> spaced name <* char '=' <*> term
      where
        packBinding (name : args) term = Label name $ foldr Abs term args
        packBinding [] _ = error "expected nonempty list"
    term = foldl1' App <$> spaced atom
    atom =
      char '\\' *> (flip (foldr Abs) <$> spaced name <* string "->" <*> term)
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
          ("trace", Eval Normal Trace <$> term),
          ("force", Eval Forced <$> tracing <*> term),
          ("show", ShowBindings <$ sc <* string "bindings" <|> return (Say ShowSyntax))
        ]
    loadMode = maybe Reset (const Append) <$> optional (space *> sym "+")
    tracing = maybe Silent (const Trace) <$> optional (space *> sym "trace")

    spaced p = sc *> sepEndBy1 p sc
    name = (:) <$> letterChar <*> many alphaNumChar
    lex = L.lexeme sc
    sym = L.symbol sc
    sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

parseBinding :: String -> Maybe Command
parseBinding s = case parseCommand s of
  Just b@(Bind _) -> Just b
  _ -> Nothing

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
    \   :trace <expr>               evaluate <expr> with tracing on\n\
    \   :force [trace] <expr>       force complete <expr> evaluation\n\n\
    \ -- Commands for displaying information:\n\n\
    \   :show bindings              \
    \show the current bindings made at the prompt\n"
