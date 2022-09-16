module Lambda (
  LambdaExpr(..),
  LambdaVal(..),
  LEnv,
) where

import Data.Text
import Data.Map

data LambdaVal
  = LRat Rational
  | LList [Rational]

type LEnv = Map Text LambdaExpr

data LambdaExpr
  = LVar Text
  | LApp LambdaExpr LambdaExpr
  | LLit LambdaVal
