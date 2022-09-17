{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralisedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Interpreter (
  eval, showVal
) where

import Control.Monad.Reader
import Data.Fix
import Data.List
import Data.Map
import Data.Ratio
import Data.Text hiding (empty)
import Lambda
import Pretty

newtype LEnv t = LEnv { unEnv :: Reader (Map Text (RuntimeVal LEnv)) t } 
  deriving (Functor, Applicative, Monad)

instance MonadReader (Map Text (RuntimeVal LEnv)) LEnv where
  ask = LEnv ask
  local f = LEnv . local f . unEnv

builtins :: Map Text (LambdaExpr -> LambdaExpr)
builtins = fromList [] -- TODO: Add builtin functions

eval :: LambdaExpr -> RuntimeVal LEnv
eval l = runReader (unEnv (foldFix exprAlgebra l)) empty

exprAlgebra :: MonadReader (Map Text (RuntimeVal m)) m
            => Algebra LambdaF (m (RuntimeVal m))
exprAlgebra = 
  \case
    LVal (LRat x) -> (pure . ComputedValue . LRat) x
    LVal (LList xs) -> ComputedValue .  LList <$> sequence xs
    LVar t -> do
      e <- ask
      pure (e ! t)
    LAbs t body -> do
      e <- ask
      pure $ Closure t e body
    LApp f x -> do
      f' <- f
      x' <- x
      case f' of
        Closure t ctx e -> local (const (Data.Map.insert t x' ctx)) e
        ComputedValue v -> (pure.ComputedValue) v

showVal :: RuntimeVal m -> String
showVal (ComputedValue (LRat x))
  | denominator x == 1 = show (numerator x) #Literal
  | otherwise          = show (numerator x) #Literal <> "/" #Parens <> show (denominator x) #Literal
showVal (ComputedValue (LList xs)) = "[" #Parens <>  Data.List.intercalate " ; " (showVal <$> xs) <> "]" #Parens
showVal Closure{} = "Cannot show for now" #Error

