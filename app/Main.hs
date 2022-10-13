module Main where

import Command
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Labeled (Labeled (Label))
import Data.StringTrie
import Data.Term hiding (App)
import System.Console.Haskeline
import System.IO (readFile)

----------------------------------- AppState -----------------------------------

data AppState = App {bindings :: StringTrie Term, lastCommand :: Command}

matchingKeys :: String -> AppState -> [String]
matchingKeys s = keys . submap s . bindings

writeBinding :: String -> Term -> AppState -> AppState
writeBinding n t (App bs lc) = App (insert n t bs) lc

writeCmd :: Command -> AppState -> AppState
writeCmd c s = s {lastCommand = c}

allBindings :: AppState -> [Labeled Term]
allBindings = map (uncurry Label) . toList . bindings

resolve' :: Term -> AppState -> Either ResolveError Term
resolve' t = flip resolve t . bindings

------------------------------------- REPL -------------------------------------

main :: IO ()
main = evalStateT (runInputT settings loop) app
  where
    app = App {bindings = empty, lastCommand = Say NoLastCommand}
    settings =
      Settings
        { complete = completeFromBindings,
          historyFile = Just ".lambda_history",
          autoAddHistory = True
        }

completeFromBindings :: CompletionFunc (StateT AppState IO)
completeFromBindings = completeWord escapeChar whitespace impl
  where
    impl :: Monad m => String -> StateT AppState m [Completion]
    impl s = map simpleCompletion . matchingKeys s <$> get
    escapeChar = Just prefix
    whitespace = " ()\\>"

include :: String -> InputT (StateT AppState IO) ()
include s = do
  s <- lift $ lift $ readFile ("./lib/" ++ s ++ ".lc")
  for_ (lines s) $ \line -> case parseBinding line of
    Nothing -> return ()
    Just (Bind (Label nm te)) -> do
      state <- lift get
      case resolve' te state of
        Left e -> outputStrLn $ show e
        Right t -> lift . modify . writeBinding nm $ bigStep (smallStep normal) t

loop :: InputT (StateT AppState IO) ()
loop = do
  line <- getInputLine "> "
  command <- lift $ case maybe (Just Quit) parseCommand line of
    Nothing -> lastCommand <$> get
    Just cmd -> modify (writeCmd cmd) $> cmd
  case command of
    (Bind (Label nm te)) -> lift get >>= onBindingResolve nm . resolve' te
    (Eval nm tr te) -> lift get >>= onEvalResolve nm tr . resolve' te
    ShowBindings -> lift get >>= replyAll . allBindings
    (Load lm ss) -> case lm of
      Reset -> reply "TODO"
      Append -> for_ ss include >> loop
    Reload -> reply "TODO"
    Say msg -> reply msg
    Quit -> outputStrLn "Leaving Lambda."
  where
    reply x = outputStrLn (show x) >> loop
    replyAll xs = for_ xs (outputStrLn . show) >> loop

    onBindingResolve _ (Left e) = reply e
    onBindingResolve nm (Right t) = do
      lift . modify . writeBinding nm $ bigStep (smallStep normal) t
      loop

    onEvalResolve _ _ (Left e) = reply e
    onEvalResolve nl em (Right t) =
      let step = smallStep $ case nl of
            Normal -> normal
            Forced -> forced
       in case em of
            Trace -> replyAll (stepLog step t)
            Silent -> reply (bigStep step t)
