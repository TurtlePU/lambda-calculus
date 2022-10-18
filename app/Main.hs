{-# LANGUAGE FlexibleContexts #-}

module Main where

import Command
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Labeled (Labeled (Label))
import Data.StringTrie
import Data.Term hiding (App)
import Data.Traversable (for)
import Parser
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

type FileName = String
type FileContents = String

data BindingError = Resolve ResolveError | Parse ParsecError deriving Show

updateBindings :: MonadState AppState m => [Labeled Term] -> m [BindingError]
updateBindings [] = return []
updateBindings ((Label nm te): xs) = do
      state <- get
      case resolve' te state of
        Left e -> do
          res <- updateBindings xs
          return (Resolve e: res)
        Right t -> (modify . writeBinding nm $ bigStep (smallStep normal) t) *> updateBindings xs

include :: MonadState AppState m => FileName -> FileContents -> m [BindingError]
include file s = do
  let (errors, bindings) = parseModule file s
  bindingErrors <- updateBindings bindings
  let parsingErrors = map Parse errors
  return $ bindingErrors ++ parsingErrors

importModule :: FileName -> InputT (StateT AppState IO) [BindingError]
importModule moduleName = do
  content <- lift $ lift $ readFile ("./lib/" ++ moduleName ++ ".lc")
  lift $ include moduleName content

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
      Append -> do
        parsingLogs <- for ss importModule
        _ <- for (zip parsingLogs ss) printErrors
        loop
    Reload -> reply "TODO"
    Say msg -> reply msg
    Quit -> outputStrLn "Leaving Lambda."
  where
    reply x = outputStrLn (show x) >> loop
    replyAll xs = for_ xs (outputStrLn . show) >> loop

    printErrors (errors, moduleName) = case concat $ map (\err -> show err ++ "\n") errors of
        [] -> return ()
        s  -> outputStrLn $ "Errors during import module " ++ moduleName ++ ":\n" ++ s

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
