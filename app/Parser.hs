{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Parser (parseModule, parseCommand, prefix) where

import Command
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Labeled (Labeled (Label))
import Data.List (foldl1')
import Data.Maybe (catMaybes)
import Data.StringTrie
import Data.Term (Term (..))
import Data.Void (Void)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

prefix :: Char
prefix = ':'

binding ::
  (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m (Labeled Term)
binding = packBinding <$> spaced name <* char '=' <*> term
  where
    packBinding (name : args) term = Label name $ foldr Abs term args
    packBinding [] _ = error "expected nonempty list"

term :: (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m Term
term = foldl1' App <$> spaced atom
  where
    atom =
      char '\\' *> (flip (foldr Abs) <$> spaced name <* string "->" <*> term)
        <|> between (char '(') (char ')') term
        <|> Var 0 <$> name

spaced :: (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m a -> m [a]
spaced p = sc *> sepEndBy1 p sc

name :: (Token s ~ Char, MonadParsec e s m) => m String
name = (:) <$> letterChar <*> many alphaNumChar

sc :: (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

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

    finishes ::
      (Token s ~ Char, Tokens s ~ String, MonadParsec e s m) =>
      StringTrie (m Command)
    finishes =
      fromList
        [ ("help", return $ Say Help),
          ("?", return $ Say Help),
          ("module", Load <$> (sc *> loadMode) <*> sepEndBy name sc),
          ("quit", return Quit),
          ("reload", return Reload),
          ("trace", Eval Normal Trace <$> term),
          ("force", Eval Forced <$> (sc *> tracing) <*> term),
          ( "show",
            ShowBindings <$ sc <* string "bindings" <|> return (Say ShowSyntax)
          )
        ]

    loadMode ::
      (Token s ~ Char, Tokens s ~ String, MonadParsec e s m) =>
      m LoadMode
    loadMode = maybe Reset (const Append) <$> optional (sym "+")

    tracing ::
      (Token s ~ Char, Tokens s ~ String, MonadParsec e s m) =>
      m Tracing
    tracing = maybe Silent (const Trace) <$> optional (sym "trace")

    sym ::
      (Token s ~ Char, Tokens s ~ String, MonadParsec e s m) =>
      String ->
      m String
    sym = L.symbol sc

type FileName = String

type FileContents = String

parseModule :: FileName -> FileContents -> ([ParsecError], [Labeled Term])
parseModule file =
  fmap catMaybes
    . partitionEithers
    . map (runParser parseModuleLine file)
    . lines
  where
    parseModuleLine = (Nothing <$ space <* eof) <|> (Just <$> binding)
