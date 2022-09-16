{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad.Trans
import Options.Applicative.Builder
import Options.Applicative.Extra
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
execCommand (CommandRepl Nothing) = runInputT defaultSettings repl
execCommand (CommandRepl (Just _)) = putStrLn ("Coming soon..." #Error)

repl :: InputT IO ()
repl = do
  getInputLine ("Dodu" #Operator ++ "> " #Parens) >>=
    \case
      Nothing -> pure ()
      Just ":q" -> lift $ putStrLn "Thanks for using Dodu 🐧"
      Just r -> lift (putStrLn r) >> repl
