module Lambda (
  LambdaExpr(..)
) where

import Data.Text

data LambdaExpr
  = LVar Text
  | LApp LambdaExpr LambdaExpr
  | LLit Rational
