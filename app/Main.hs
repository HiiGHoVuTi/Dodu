{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Map
import Data.Text (unpack)
import Interpreter
import Options.Applicative.Builder
import Options.Applicative.Extra
import Parser
import Pretty
import System.Console.Haskeline

data Command
   = CommandHi
   | CommandRepl (Maybe FilePath)

main :: IO ()
main = execParser opts >>= execCommand 
  where
    opts = info (optsParser <**> helper)
         $ fullDesc
        <> progDesc (unlines
          [  
          ])
        <> header "Dodu, your friendly combinatory language toolkit"

    optsParser =
      subparser
        ( command "say-hi" (info (pure CommandHi) (progDesc "Say hi to Dodu !"))
       <> command "repl"   (info replCommand      (progDesc "Open a repl to start experimenting"))
        )
        
    replCommand = CommandRepl <$> (optional . strOption)
      (  long "load"
      <> metavar "INPUT"
      <> help "the path to the file to load in the repl"
      )

settings :: Settings (ReaderT Scope IO)
settings = Settings
  -- TODO: Something moe sophisticated
  { complete = completeWord Nothing "\t \n" $ \startOfWord -> do
      inScope <- asks (fmap unpack . Data.Map.keys)
      let possibleWords = Prelude.filter (isPrefixOf startOfWord) (fmap unpack builtinNames ++ inScope)
          completions = fmap (\w -> Completion
            { replacement = w
            , display = w
            , isFinished = True
            }) possibleWords
      pure completions
  -- TODO: history file ?
  , historyFile = Nothing
  , autoAddHistory = True
  }

execCommand :: Command -> IO ()
execCommand CommandHi = putStrLn ("Hi, Dodu !" #Field)
execCommand (CommandRepl Nothing) = flip runReaderT empty $ runInputT settings repl
execCommand (CommandRepl (Just filepath)) = do
  contents <- readFile filepath
  case parseProgram filepath contents of
    Left e -> print e
    Right xs -> 
      case foldM run empty xs of
        Left e -> print e
        Right scope -> flip runReaderT scope $ runInputT settings repl
    where
      run m (i, p) =
        case eval m p of
          Left e -> Left e
          Right v -> Right $ Data.Map.insert i v m

repl :: InputT (ReaderT Scope IO) ()
repl = do
  getInputLine ("Dodu" #Operator ++ "> " #Parens) >>=
    \case
      Nothing -> pure ()
      Just "" -> repl
      Just ":q" -> lift.lift $ putStrLn "Thanks for using Dodu 🐧"
      Just r ->
        let asStr = Prelude.take 2 r == ":s"
            r' = if asStr then Prelude.drop 2 r else r
            showV = if asStr then valString else showVal 16
        in case parseProgram "repl" r' of
          -- not a program, so maybe an expr
          Left _ ->
            case parseExpr "repl" r' of
              Left e -> (lift.lift) (print e) >> repl
              Right p -> do 
                scope <- lift ask
                case eval scope p of
                  Right v -> (lift.lift . putStrLn . showV) v >> repl
                  Left e -> (lift.lift . putStrLn . unpack) e >> repl
          Right xs -> do
            scope <- lift ask
            case sequence $ eval scope <$> fromList xs of
              Right newScope -> mapInputT (local (Data.Map.union newScope)) repl
              Left e -> (lift.lift . putStrLn . unpack) e >> repl
