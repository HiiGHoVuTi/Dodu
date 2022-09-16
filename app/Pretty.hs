{-# LANGUAGE OverloadedStrings #-}

module Pretty (
  (#), Color(..)
              ) where

-- | The enum of all possible colors (ANSI terminal)
data Color
  = Normal
  | CUnit
  | Literal
  | Field
  | Parens
  | Operator
  | Error

-- | ANSI-escapes a color, coloring every subsequent character printed
escape :: Color -> String
escape col = "\ESC[" <> escape' col <> "m"

-- | A map of all ANSI Color-Codes
escape' :: Color -> String
escape' Normal   = "0"
escape' Operator = "0;35"
escape' CUnit    = "0;31"
escape' Literal  = "0;32"
escape' Field    = "4;36"
escape' Parens   = "0;33"
escape' Error    = "1;31"

-- | An operator which colors a certain string, and sets back the terminal color to normal afterwards
(#) :: String -> Color -> String
(#) src col = escape col <> src <> escape Normal

