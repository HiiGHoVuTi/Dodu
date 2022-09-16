module Lambda (
  LambdaExpr(..),
  LEnv,
) where

import Data.Text
import Data.Map

data LambdaVal
  = Rational
  | List Rational

data LEnv = Map Text LambdaExpr

data LambdaExpr
  = LVar Text
  | LApp LambdaExpr LambdaExpr
  | LLit LambdaVal
