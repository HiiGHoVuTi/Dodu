{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralisedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds #-}
module Interpreter (
  eval, showVal
) where

import Control.Monad
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

type IsEnv m = MonadReader (Map Text (RuntimeVal m)) m

instance MonadReader (Map Text (RuntimeVal LEnv)) LEnv where
  ask = LEnv ask
  local f = LEnv . local f . unEnv

executeAsDataFunction :: IsEnv m => RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
executeAsDataFunction f x = 
  case f of 
    ComputedValue z -> pure (ComputedValue z)
    DataFunction g -> g x

concatValues :: RuntimeVal m -> RuntimeVal m -> RuntimeVal m
concatValues a'@(ComputedValue a) b'@(ComputedValue b) =
  ComputedValue $ case (a, b) of
    (LRat _, LRat _) -> LList [a', b']
    (LList xs, LRat _) -> LList (xs ++ [b'])
    (LRat _, LList ys) -> LList (a' : ys)
    (LList xs, LList ys) -> LList (xs ++ ys)
concatValues _ _ = error ("sale rat" #Error)

rankPolymorphicBinary' ::
  IsEnv m =>
  (Rational -> Rational -> m Rational) ->
  RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
rankPolymorphicBinary' f a' b' =
  case (a', b') of
    (ComputedValue a, ComputedValue b) -> ComputedValue <$>
      case (a, b) of
        (LRat x, LRat y) -> LRat <$> f x y
        (LRat _, LList y) -> LList <$> traverse (rankPolymorphicBinary' f a') y
        (LList x, LRat _) -> LList <$> traverse (flip (rankPolymorphicBinary' f) b') x
        (LList x, LList y) -> LList <$> zipWithM (rankPolymorphicBinary' f) x y
    _ -> error ("sale rat" #Error)

rankPolymorphicBinary :: IsEnv m => (Rational -> Rational -> m Rational) -> RuntimeVal m
rankPolymorphicBinary f =
  DataFunction $ \a -> pure . DataFunction $ \b ->
  rankPolymorphicBinary' f a b

builtins :: IsEnv m => Map Text (RuntimeVal m)
builtins = fromList
  [ ("I" , DataFunction pure)
  , ("K" , DataFunction . const . pure $ DataFunction pure)
  , ("C" , DataFunction $ \f -> pure . DataFunction $ \x -> pure . DataFunction $ \y ->
      do g <- executeAsDataFunction f y; executeAsDataFunction g x)
  , ("+" , rankPolymorphicBinary $ (pure .) . (+))
  , ("-" , rankPolymorphicBinary $ (pure .) . (-))
  , ("*" , rankPolymorphicBinary $ (pure .) . (*))
  , ("/" , rankPolymorphicBinary $ (pure .) . (/))
  , ("=" , rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (==))
  , ("!=", rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (/=))
  , (">" , rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (>))
  , (">=" , rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (>=))
  , ("<" , rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (<))
  , ("<=" , rankPolymorphicBinary $ ((pure . toEnum . fromEnum) .) . (<=))
  , ("i" , DataFunction $ \(ComputedValue (LRat x)) -> pure . ComputedValue . LList . fmap (ComputedValue . LRat) $ [0..x])
  , (":" , DataFunction $ \x -> pure . DataFunction $ \y -> pure (concatValues x y))
  , ("map", DataFunction $ \f -> pure . DataFunction $ \x'@(ComputedValue x) -> 
    case x of
      LRat _ -> executeAsDataFunction f x'
      LList xs -> ComputedValue . LList <$> traverse (executeAsDataFunction f) xs)
  , ("keep", DataFunction $ \f -> pure . DataFunction $ \x'@(ComputedValue x) -> 
    ComputedValue <$> case x of
      LRat _ -> do 
        y <- executeAsDataFunction f x'
        let ComputedValue (LRat p) = y 
        pure . LList $ [x' | p > 0]
      LList xs -> do
        ps <- traverse (executeAsDataFunction f) xs
        pure . LList $ [v | (v, ComputedValue (LRat p)) <- Data.List.zip xs ps, p > 0])
  , ("fold", DataFunction $ \f -> pure . DataFunction $ \(ComputedValue x) -> 
    case x of
      LRat v -> pure . ComputedValue . LRat $ v
      LList xs -> Data.List.foldl' (\a b -> do
        g <- executeAsDataFunction f =<< a
        executeAsDataFunction g b) (pure $ Data.List.head xs) (Data.List.tail xs))
  , ("scan", DataFunction $ \f -> pure . DataFunction $ \x'@(ComputedValue x) -> 
    ComputedValue <$> case x of
      LRat _ -> pure . LList $ [x']
      LList xs -> fmap LList . sequence $ Data.List.scanl' (\a b -> do
        g <- executeAsDataFunction f =<< a
        executeAsDataFunction g b) (pure $ Data.List.head xs) (Data.List.tail xs))
  ]

-- NOTE(Maxime): this is actually needed for some reason
builtinNames :: [Text]
builtinNames
  =  -- Combinators
  ["I", "K", "C"]
  ++ -- Numbers
  ["+", "-", "*", "/"]
  ++ -- Comparison
  ["=", "!=", ">", ">=", "<", "<="]
  ++ -- Arrays
  ["i", ":"] -- TODO: head, tail, take, rotate

eval :: LambdaExpr -> RuntimeVal LEnv
eval l = runReader (unEnv (foldFix exprAlgebra l)) empty

exprAlgebra :: IsEnv m => Algebra LambdaF (m (RuntimeVal m))
exprAlgebra =
  \case
    LVal (LRat x) -> (pure . ComputedValue . LRat) x
    LVal (LList xs) -> ComputedValue .  LList <$> sequence xs
    LVar t
      | t `Data.List.elem` builtinNames -> pure $ builtins ! t
      | otherwise -> do
        e <- ask
        pure (e ! t)
    LAbs t body -> do
      e <- ask
      pure . DataFunction $ \x -> do
        local (const (Data.Map.insert t x e)) body
    LApp f' x' -> do
      f <- f'; x <- x'
      case f of
        -- Closure t ctx e -> local (const (Data.Map.insert t x ctx)) e
        DataFunction df -> df x
        ComputedValue v -> (pure.ComputedValue) v

showVal :: RuntimeVal m -> String
showVal (ComputedValue (LRat x))
  | denominator x == 1 = show (numerator x) #Literal
  | otherwise          = show (numerator x) #Literal <> "/" #Parens <> show (denominator x) #Literal
showVal (ComputedValue (LList xs)) = "[" #Parens <>  Data.List.intercalate " ; " (showVal <$> xs) <> "]" #Parens
-- showVal Closure{}      = "Cannot show Closure" #Error
showVal DataFunction{} = "Cannot show Data Function" #Error
