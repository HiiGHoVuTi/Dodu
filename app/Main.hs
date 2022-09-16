module Main where

import Control.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra

data Command = CommandHi

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
        )

execCommand :: Command -> IO ()
execCommand CommandHi = putStrLn "Hi, Dodu !"
