{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Parser (parseModule, parseCommand, prefix) where

import Command
import Data.Char (isSpace)
import Data.Labeled (Labeled (Label))
import Data.List (foldl1')
import Data.StringTrie
import Data.Term (Term (..))
import Data.Void (Void)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

prefix :: Char
prefix = ':'

binding :: (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m (Labeled Term)
binding = (packBinding <$> spaced name <* char '=' <*> term)
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
    
    sym = L.symbol sc

type FileName = String
type FileContents = String

shrink :: [Either a b] -> ([a], [b])
shrink [] = ([], [])
shrink (Left a: xs) = let (as, bs) = shrink xs in ((a: as), bs)
shrink (Right b: xs) = let (as, bs) = shrink xs in (as, (b: bs))

parseModuleLine :: (Token s ~ Char, Tokens s ~ [Char], MonadParsec e s m) => m [Labeled Term]
parseModuleLine = ((\_ -> []) <$> (space <* eof)) <|> ((\a -> [a]) <$> binding)

parseModule :: FileName -> FileContents -> ([ParsecError], [Labeled Term])
parseModule file s = let (a, b) = shrink $ map (\s -> runParser parseModuleLine file s) (lines s)
    in (a, concat b)
