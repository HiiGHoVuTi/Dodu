{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralisedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds #-}
module Interpreter (
  Scope,
  eval, showVal, valString
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Fix
import Data.List
import Data.Map
import Data.Ratio
import Data.Text hiding (empty, head, tail)
import Lambda
import Pretty

type Scope = Map Text (RuntimeVal LEnv)

type Rat = Ratio Int

newtype LEnv t = LEnv { unEnv :: ExceptT Text (Reader Scope) t }
  deriving (Functor, Applicative, Monad)

type IsEnv m = (MonadReader (Map Text (RuntimeVal m)) m, MonadError Text m)

instance MonadReader (Map Text (RuntimeVal LEnv)) LEnv where
  ask = LEnv ask
  local f = LEnv . local f . unEnv

instance MonadError Text LEnv where
  throwError = LEnv . throwError
  catchError (LEnv a) f = LEnv $ catchError a (unEnv.f)

rvToMa :: MonadError Text m => RuntimeVal a -> m (MultiArray Rat)
rvToMa  DataFunction{} = throwError $ pack ("Cannot use function as value" #Error)
rvToMa (ComputedValue (LRat v)) = pure (Single v) 
rvToMa (ComputedValue (LList xs)) = fmap Many . traverse rvToMa $ xs

maToRv :: MultiArray Rat -> RuntimeVal m
maToRv (Single v) = ComputedValue (LRat v)
maToRv (Many xs) = ComputedValue . LList . fmap maToRv $ xs

onMultiArray 
  :: IsEnv m 
  => (MultiArray Rat -> m (MultiArray Rat))
  -> RuntimeVal m
  -> m (RuntimeVal m)
onMultiArray f x = maToRv <$> (rvToMa x >>= f)

asmaf :: IsEnv m 
  => (RuntimeVal m -> m (RuntimeVal m))
  -> MultiArray Rat -> m (MultiArray Rat)
asmaf f m = f (maToRv m) >>= rvToMa

executeAsDataFunction :: IsEnv m => RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
executeAsDataFunction f x =
  case f of
    ComputedValue z -> pure (ComputedValue z)
    DataFunction g -> g x

-- TODO(Maxime): implement with onMultiArray
concatValues :: IsEnv m => RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
concatValues a'@(ComputedValue a) b'@(ComputedValue b) =
  pure . ComputedValue $ case (a, b) of
    (LRat _, LRat _) -> LList [a', b']
    (LList xs, LRat _) -> LList (xs ++ [b'])
    (LRat _, LList ys) -> LList (a' : ys)
    (LList xs, LList ys) -> LList (xs ++ ys)
concatValues _ _ = throwError $ pack ("sale rat" #Error)

-- TODO(Maxime): implement with onMultiArray
rankPolymorphicBinary' ::
  IsEnv m =>
  (Rat -> Rat -> m (RuntimeVal m)) ->
  RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
rankPolymorphicBinary' f a' b' =
  case (a', b') of
    (ComputedValue a, ComputedValue b) ->
      case (a, b) of
        (LRat x, LRat y) -> f x y
        (LRat _, LList y) -> ComputedValue . LList <$> traverse (rankPolymorphicBinary' f a') y
        (LList x, LRat _) -> ComputedValue . LList <$> traverse (flip (rankPolymorphicBinary' f) b') x
        (LList x, LList y) -> ComputedValue . LList <$> zipWithM (rankPolymorphicBinary' f) x y
    _ -> throwError $ pack ("sale rat" #Error)

rankPolymorphicBinary :: IsEnv m => (Rat -> Rat -> m (RuntimeVal m)) -> RuntimeVal m
rankPolymorphicBinary f =
  DataFunction $ \a -> pure . DataFunction $ \b ->
  rankPolymorphicBinary' f a b

builtins :: IsEnv m => Map Text (RuntimeVal m)
builtins = fromList
  [ ("I" , DataFunction pure)
  , ("K" , DataFunction . const . pure $ DataFunction pure)
  , ("C" , DataFunction $ \f -> pure . DataFunction $ \x -> pure . DataFunction $ \y ->
      do g <- executeAsDataFunction f y; executeAsDataFunction g x)
  , ("D", DataFunction $ \a -> pure . DataFunction $ \b -> pure . DataFunction $ \c -> pure . DataFunction $ \d ->
      do g <- executeAsDataFunction a b; h <- executeAsDataFunction c d; executeAsDataFunction g h)
  , ("B", DataFunction $ \a -> pure . DataFunction $ \b -> pure . DataFunction $ \c ->
      do g <- executeAsDataFunction b c; executeAsDataFunction a g)

  , ("+" , rankPolymorphicBinary $ (pureRat .) . (+))
  , ("-" , rankPolymorphicBinary $ (pureRat .) . (-))
  , ("*" , rankPolymorphicBinary $ (pureRat .) . (*))
  , ("/" , rankPolymorphicBinary $ (pureRat .) . (/))
  , ("=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (==))
  , ("!=", rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (/=))
  , (">" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (>))
  , (">=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (>=))
  , ("<" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (<))
  , ("<=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (<=))
  , ("i" , DataFunction $ \(ComputedValue (LRat x)) -> pure . ComputedValue . LList . fmap (ComputedValue . LRat) $ [0..x])
  , ("::", rankPolymorphicBinary $ \x y -> pure . ComputedValue . LList $ [ComputedValue (LRat x), ComputedValue (LRat y)])
  , (":" , DataFunction $ \x -> pure . DataFunction $ \y -> concatValues x y)
  , ("numerator", DataFunction $ onMultiArray $ pure . fmap ((%1).numerator))
  , ("denominator", DataFunction $ onMultiArray $ pure . fmap ((%1).denominator))

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
  
  , ("outer", DataFunction $ \f' -> 
       pure . DataFunction $ \a' -> pure . DataFunction $ \b' -> do 
          a <- rvToMa a' ; b <- rvToMa b'
          let f x y = f' `executeAsDataFunction` ComputedValue (LRat x) 
                 >>= flip executeAsDataFunction (ComputedValue (LRat y))
                 >>= rvToMa
          z <- sequenceA $ f <$> a <*> b
          pure . maToRv . foldMultiArray $ z)
  
  -- FIXME(Maxime): use throwError
  , ("transpose", DataFunction . onMultiArray $ \x ->
    let asList (Single _) = error ("Cannot transpose 1D-Array" #Error)
        asList (Many xs)  = xs
    in case x of
      Single v -> pure (Single v)
      Many xs -> pure . Many . fmap Many . Data.List.transpose . fmap asList $ xs)

  , ("head", DataFunction $ \x'@(ComputedValue x) ->
    case x of
      LRat _ -> pure x'
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure (head xs))
  , ("last", DataFunction $ \x'@(ComputedValue x) ->
    case x of
      LRat _ -> pure x'
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure (Data.List.last xs))

  , ("tail", DataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case x of
      LRat _ -> pure . LList $ []
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure . LList . tail $ xs)

  , ("take", DataFunction $ \(ComputedValue n') -> pure . DataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case (n', x) of
      (LRat _, LRat _) -> pure x
      (LRat n, LList xs) -> pure . LList $ Data.List.take (fromEnum n) xs
      (_, _) -> throwError $ pack ("Bad arguments" #Error))

  , ("rotate", DataFunction $ \(ComputedValue n') -> pure . DataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case (n', x) of
      (LRat n, LList xs) -> pure . LList $ rotate (fromEnum n) xs
                            where rotate :: Int -> [a] -> [a]
                                  rotate a l = Data.List.zipWith const (Data.List.drop a (cycle l)) l
      (_, _) -> throwError $ pack ("Bad arguments" #Error))

  , ("rev", DataFunction $ \x ->
    case x of
      ComputedValue (LRat _) -> pure x
      ComputedValue (LList xs) -> pure . ComputedValue . LList . Data.List.reverse $ xs
      _ -> throwError $ pack ("Cannot reverse function" #Error))

  , ("flat", DataFunction $ let 
        flat = pure . Many . Prelude.foldr ((:).Single) []
      in onMultiArray flat)

  -- TODO: x ∈ xs
  -- TODO: reshape ρ

  -- TODO(Maxime): implement with onMultiArray
  -- FIXME(Maxime): use throwError
  , ("nth", DataFunction $ \n' -> pure . DataFunction $ \x ->
    let
      select [] xs = xs
      select (n:ns) xs =
        case xs of
          ComputedValue (LList xs') ->
            case n of
              ComputedValue (LRat i) -> 
                case (ns, xs' !! fromEnum i) of
                  ([], z) -> z
                  (_, z@(ComputedValue (LList _))) -> select ns z
                  _ -> error ("Indexing a Rat" #Error)
              _ -> error ("Please index with a Rat" #Error)
          z@(ComputedValue (LRat _)) -> z 
          _ -> error ("Indexing a function" #Error)
    in pure $ case (n', x) of
      (ComputedValue (LRat ___), ComputedValue (LList _)) -> select [n'] x
      (ComputedValue (LList ns), ComputedValue (LList _)) -> select ns x
      (_, _) -> error ("Bad arguments" #Error))
      
  , ("sort", DataFunction $ \x'@(ComputedValue x) -> 
    case x of
      LRat _ -> pure x'
      LList xs -> pure . ComputedValue . LList $ sortOn (\(ComputedValue (LRat v)) -> v) xs)
  , ("nub", DataFunction . onMultiArray $
      let isMany (Many _) = True ; isMany _ = False
          nub' (Single v) = pure (Single v)
          nub' (Many xs)
            | Prelude.null xs = pure $ Many []
            | isMany (Prelude.head xs) = Many <$> traverse nub' xs
            | otherwise = pure . Many $ nub xs
        in nub')
  , ("iter", DataFunction $ \(ComputedValue (LRat n)) -> 
      pure . DataFunction $ \(DataFunction f) -> 
      pure . DataFunction $ \x'@(ComputedValue _) ->
        fmap (ComputedValue . LList) . sequence $ Data.List.take (fromEnum n) (iterate (f =<<) $ pure x'))
  ]
  where pureRat = pure . ComputedValue . LRat

-- NOTE(Maxime): this is actually needed for some reason
builtinNames :: [Text]
builtinNames
  =  -- Combinators
  ["I", "K", "C", "D", "B"]
  ++ -- Numbers
  ["+", "-", "*", "/"] ++ ["numerator", "denominator"]
  ++ -- Comparison
  ["=", "!=", ">", ">=", "<", "<="]
  ++ -- Folds, unfolds, maps
  ["map", "fold", "scan", "iter", "head", "last", "tail", "take" 
  ,"rotate", "rev", "transpose", "flat", "nub"]
  ++ -- misc ?
  ["nth", "keep", "sort"]
  ++ -- Arrays
  ["i", ":", "::", "outer"]

eval :: Scope -> LambdaExpr -> Either Text (RuntimeVal LEnv)
eval m l = runReader (runExceptT (unEnv (foldFix exprAlgebra l))) m

exprAlgebra :: IsEnv m => Algebra LambdaF (m (RuntimeVal m))
exprAlgebra =
  \case
    LVal (LRat x) -> (pure . ComputedValue . LRat) x
    LVal (LList xs) -> ComputedValue .  LList <$> sequence xs
    LVar t
      | t `Data.List.elem` builtinNames -> pure $ builtins ! t
      | otherwise -> do
        e <- ask
        if t `member` e
          then pure (e ! t)
          else throwError $ pack (("No function named " ++ unpack t) #Error)
    LAbs t body -> do
      e <- ask
      pure . DataFunction $ \x -> do
        local (const (Data.Map.insert t x e)) body
    LApp f' x' -> do
      f <- f'; x <- x'
      case f of
        DataFunction df -> df x
        ComputedValue v -> (pure.ComputedValue) v

showVal :: RuntimeVal m -> String
showVal x = case rvToMa x of
  Left _ -> "Cannot show Data Function" #Error
  Right m -> render m
  where
    length' = flip length'' True
    length'' "" _ = 0
    length'' "\n" _ = 0 
    length'' ('\ESC':zs) True = length'' zs False
    length'' (_:zs) True = 1 + length'' zs True
    length'' ('m':zs) False = length'' zs True
    length'' (_:zs) False = length'' zs False
    
    borders = Parens
    
    depth (Single _) = 0 :: Int; depth (Many []) = 1; depth (Many xs) = 1 + Data.List.minimum (depth <$> xs)
    
    render (Single v)
      | denominator v == 1 = show (numerator v) #Literal
      | otherwise          = show (numerator v) #Literal <> "/" #Operator <> show (denominator v) #Literal
    render v@(Many xs)
      | depth v == 1 = let
          els  = render <$> xs
          text = Data.List.intercalate (" │ " #borders) els
          vert c = Data.List.intercalate c
            [ Data.List.replicate (length' e + 2) '─' | e <- els ]
            ++ if Prelude.null els then "──" else ""
        in ("╭"  ++ vert "┬" ++  "╮\n") #borders
        ++ "│ " #borders ++ text ++ " │\n" #borders
        ++ ("╰"  ++ vert "┴" ++  "╯") #borders
      | depth v == 2 = let
          toList' (Single _) = error "unreachable"; toList' (Many z) = z
          mxL = Data.List.maximum . fmap Data.List.length $ xs
          pad zs = zs ++ Data.List.replicate (mxL - Prelude.length zs) ""
          mat = fmap (pad . fmap ((++ " ").render)) $ toList' <$> xs
          widths = fmap Data.List.maximum . Data.List.transpose 
            $ (fmap.fmap) length' mat
          vert c = Data.List.intercalate c
            [ Data.List.replicate (w+1) '─' | (w,_) <- Prelude.zip widths (head mat) ]
        in ("╭" ++ vert "┬"  ++ "╮\n") #borders
        ++ Data.List.intercalate (("\n├" ++ vert "┼" ++ "┤\n") #borders) 
          ["│ " #borders ++ Data.List.intercalate ("│ " #borders) 
            [Data.List.replicate (w - length' r) ' ' ++ r | (w, r) <- Prelude.zip widths row] 
          ++  "│" #borders | row <- mat] ++ "\n"
        ++ ("╰"  ++ vert "┴" ++  "╯") #borders
      | otherwise = render (head xs) ++ "\n(" #Parens
          ++ "only showing first slice" #Field ++ ")" #Parens
{- 
showVal (ComputedValue (LRat x))
  | denominator x == 1 = show (numerator x) #Literal
  | otherwise          = show (numerator x) #Literal <> "/" #Parens <> show (denominator x) #Literal
showVal (ComputedValue (LList xs)) = "[" #Parens <>  Data.List.intercalate " ; " (showVal <$> xs) <> "]" #Parens
showVal DataFunction{} = "Cannot show Data Function" #Error
-}

valString :: RuntimeVal m -> String
valString (ComputedValue (LRat x)) = pure (toEnum $ fromEnum x)
valString (ComputedValue (LList xs))
  | Prelude.null xs = "[]" #Parens
  | isList (Prelude.head xs) = "[" #Parens <>  Data.List.intercalate " ; " (valString <$> xs) <> "]" #Parens
  | otherwise = Prelude.concatMap valString xs
  where isList (ComputedValue (LList _)) = True ; isList _ = False
valString DataFunction{} = "Cannot show Data Function" #Error
