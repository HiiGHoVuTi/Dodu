{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative hiding (empty)
import Control.Monad.Trans
import Data.Map
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
execCommand (CommandRepl (Just _)) = putStrLn ("Coming soon..." #Error)

repl :: Scope -> InputT IO ()
repl scope = do
  getInputLine ("Dodu" #Operator ++ "> " #Parens) >>=
    \case
      Nothing -> pure ()
      Just "" -> repl scope
      Just ":q" -> lift $ putStrLn "Thanks for using Dodu 🐧"
      Just r ->
        case parseProgram "repl" r of
          -- not a program, so maybe an expr
          Left _ ->
            case parseExpr "repl" r of
              Left e -> lift (print e)
              Right p -> (lift . putStrLn . showVal) (eval scope p)
                >> repl scope
          Right xs -> repl . flip Data.Map.union scope $ (eval scope <$> fromList xs)
