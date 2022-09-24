{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans
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

execCommand :: Command -> IO ()
execCommand CommandHi = putStrLn ("Hi, Dodu !" #Field)
execCommand (CommandRepl Nothing) = runInputT defaultSettings (repl empty)
execCommand (CommandRepl (Just filepath)) = do
  contents <- readFile filepath
  case parseProgram filepath contents of
    Left e -> print e
    Right xs -> 
      case foldM run empty xs of
        Left e -> print e
        Right scope -> runInputT defaultSettings (repl scope)
    where
      run m (i, p) =
        case eval m p of
          Left e -> Left e
          Right v -> Right $ Data.Map.insert i v m

repl :: Scope -> InputT IO ()
repl scope = do
  getInputLine ("Dodu" #Operator ++ "> " #Parens) >>=
    \case
      Nothing -> pure ()
      Just "" -> repl scope
      Just ":q" -> lift $ putStrLn "Thanks for using Dodu 🐧"
      Just r ->
        let asStr = Prelude.take 2 r == ":s"
            r' = if asStr then Prelude.drop 2 r else r
            showV = if asStr then valString else showVal 16
        in case parseProgram "repl" r' of
          -- not a program, so maybe an expr
          Left _ ->
            case parseExpr "repl" r' of
              Left e -> lift (print e)
                >> repl scope
              Right p -> 
                case eval scope p of
                  Right v -> (lift . putStrLn . showV) v
                    >> repl scope
                  Left e -> (lift . putStrLn . unpack) e
                    >> repl scope
          Right xs -> 
            case sequence $ eval scope <$> fromList xs of
              Right newScope -> repl $ Data.Map.union newScope scope
              Left e -> (lift . putStrLn . unpack) e
                    >> repl scope