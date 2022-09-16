module Parser (
  programParser, exprParser
) where

import Data.Text
import Lambda
import Text.Parsec

type Program = [(Text, LambdaExpr)]

programParser = undefined
exprParser = undefined

