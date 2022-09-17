module Lambda (
  LambdaExpr(..),
  LambdaVal(..),
  LEnv,
) where

import Data.Text
import Data.Map

data LambdaVal
  = LRat Rational -- jus de rat
  | LList [LambdaExpr]
  deriving (Show)

type LEnv = Map Text LambdaExpr

data LambdaExpr
  = LVar Text
  | LApp LambdaExpr LambdaExpr
  | LAbs Text LambdaExpr
  | LVal LambdaVal
  deriving (Show)
