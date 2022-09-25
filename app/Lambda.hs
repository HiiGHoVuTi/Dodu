{-# LANGUAGE DeriveTraversable #-}
module Lambda (
  Algebra,
  LambdaExpr, LambdaF(..),
  RuntimeVal(..), LambdaVal(..),
  MultiArray(..), foldMultiArray,
  lVal, lAbs, lApp, lVar,
) where

import Data.Fix
import Data.RatioInt
import Data.Text

type Algebra f a = f a -> a

data LambdaVal a
  = LRat RatioInt -- jus de rat
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
  | DataFunction (RuntimeVal m -> m (RuntimeVal m))
  
data MultiArray a
  = Single a
  | Many [MultiArray a]
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (MultiArray a) where
  Single a == Single b = a == b
  Many xs == Many ys   = Prelude.all (uncurry (==)) $ Prelude.zip xs ys 
  _ == _ = False

instance Applicative MultiArray where
  pure = Single
  (Single f) <*> (Single g) = Single (f g)
  (Many fs) <*> g = Many [f <*> g | f <- fs]
  f <*> (Many gs) = Many [f <*> g | g <- gs]

foldMultiArray :: MultiArray (MultiArray a) -> MultiArray a
foldMultiArray (Single (Single v)) = Single v
foldMultiArray (Single (Many xs))  = Many xs
foldMultiArray (Many xs) = Many . fmap foldMultiArray $ xs 
