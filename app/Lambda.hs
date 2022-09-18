{-# LANGUAGE DeriveFunctor #-}
module Lambda (
  Algebra,
  LambdaExpr, LambdaF(..),
  RuntimeVal(..), LambdaVal(..),
  lVal, lAbs, lApp, lVar,
) where

import Data.Fix
import Data.Map
import Data.Text

type Algebra f a = f a -> a

data LambdaVal a
  = LRat Rational -- jus de rat
  | LList [a]
  deriving (Show, Functor)

data LambdaF a
  = LVar Text
  | LAbs Text a
  | LApp a a
  | LVal (LambdaVal a)
  deriving (Show, Functor)

type LambdaExpr = Fix LambdaF
lVar :: Text -> LambdaExpr
lVar = wrapFix . LVar
lAbs :: Text -> LambdaExpr -> LambdaExpr
lAbs t e = wrapFix $ LAbs t e
lApp :: LambdaExpr -> LambdaExpr -> LambdaExpr
lApp a b = wrapFix $ LApp a b
lVal :: LambdaVal LambdaExpr -> LambdaExpr
lVal = wrapFix . LVal

data RuntimeVal m
  = ComputedValue (LambdaVal (RuntimeVal m))
  | Closure Text (Map Text (RuntimeVal m)) (m (RuntimeVal m))
  | DataFunction (RuntimeVal m -> m (RuntimeVal m))
