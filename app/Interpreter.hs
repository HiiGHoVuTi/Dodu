{-# LANGUAGE OverloadedStrings, NumericUnderscores, LambdaCase, GeneralisedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds #-}
module Interpreter (
  Scope,
  eval, showVal, valString, renderAsCsv,
  builtinNames
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Fix
import Data.List
import Data.Map
import Data.Ratio -- Int
import Data.Text hiding (empty, head, tail)
import Lambda
import Numeric
import Pretty
import System.Random.MWC
import System.IO.Unsafe

type Scope = Map Text (RuntimeVal LEnv)

type Rat = Rational

stringConstant :: Rat
stringConstant = 1 % 10_000_000_000_000_001

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
rvToMa  Delayed{} = throwError $ pack ("Cannot use function as value" #Error)
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

executeDelayed :: IsEnv m => Text -> m (RuntimeVal m) -> RuntimeVal m -> m (RuntimeVal m)
executeDelayed t f x = local (Data.Map.insert t x) f

dataFunction :: IsEnv m => (RuntimeVal m -> m (RuntimeVal m)) -> RuntimeVal m
dataFunction f = Delayed t $ asks (Data.Map.! t) >>= f
  where
    t = pack.show $ unsafePerformIO (uniformM =<< createSystemRandom :: IO Int)

executeAsDataFunction :: IsEnv m => RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
executeAsDataFunction f x =
  case f of
    ComputedValue z -> pure (ComputedValue z)
    Delayed t g -> executeDelayed t g x

-- TODO(Maxime): implement with onMultiArray
concatValues :: IsEnv m => RuntimeVal m -> RuntimeVal m -> m (RuntimeVal m)
concatValues a'@(ComputedValue a) b'@(ComputedValue b) =
  pure . ComputedValue $ case (a, b) of
    (LList (ComputedValue (LRat v):_), LRat _)
      | v == stringConstant -> LList [a', b']
    (LList (ComputedValue (LRat v):_), LList xs)
      | v == stringConstant -> LList (a' : xs)
    (LRat _, LList (ComputedValue (LRat v):_))
      | v == stringConstant -> LList [a', b']
    (LList xs, LList (ComputedValue (LRat v):_))
      | v == stringConstant -> LList (xs ++ pure b')
    (LRat _, LRat _) -> LList [a', b']
    (LList xs, LRat _) -> LList (xs ++ pure b')
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
  dataFunction $ \a -> pure . dataFunction $ \b ->
  rankPolymorphicBinary' f a b

rationalPow :: Rat -> Rat -> Rat
rationalPow a b = realToFrac $ (fromRational a :: Double) ** fromRational b

builtins :: IsEnv m => Map Text (RuntimeVal m)
builtins = fromList
  [ ("I" , dataFunction pure)
  , ("K" , dataFunction $ \x -> pure . dataFunction $ const (pure x))
  , ("C" , dataFunction $ \f -> pure . dataFunction $ \x -> pure . dataFunction $ \y ->
      do g <- executeAsDataFunction f y; executeAsDataFunction g x)
  , ("D", dataFunction $ \a -> pure . dataFunction $ \b -> pure . dataFunction $ \c -> pure . dataFunction $ \d ->
      do g <- executeAsDataFunction a b; h <- executeAsDataFunction c d; executeAsDataFunction g h)
  , ("B", dataFunction $ \a -> pure . dataFunction $ \b -> pure . dataFunction $ \c ->
      do g <- executeAsDataFunction b c; executeAsDataFunction a g)
  , ("M", dataFunction $ \a -> pure . dataFunction $ \b ->
      do ab <- executeAsDataFunction a b; executeAsDataFunction ab b)

  , ("+" , rankPolymorphicBinary $ (pureRat .) . (+))
  , ("-" , rankPolymorphicBinary $ (pureRat .) . (-))
  , ("*" , rankPolymorphicBinary $ (pureRat .) . (*))
  , ("/" , rankPolymorphicBinary $ (pureRat .) . (/))
  , ("pow" , rankPolymorphicBinary $ (pureRat .) . rationalPow)
  , ("=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (==))
  , ("!=", rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (/=))
  , (">" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (>))
  , (">=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (>=))
  , ("<" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (<))
  , ("<=" , rankPolymorphicBinary $ ((pureRat . toEnum . fromEnum) .) . (<=))
  , ("if", let
      p = pack.show $ unsafePerformIO (uniformM =<< createSystemRandom :: IO Int)
      isTruthy (ComputedValue (LRat x)) = x > 0
      isTruthy (ComputedValue (LList xs)) = Prelude.all isTruthy xs
      isTruthy _ = False
    in Delayed p $ 
      asks (isTruthy . (Data.Map.! p)) >>= \case
        True  -> pure . dataFunction $ \t -> pure (Delayed "" (pure t)) 
        False -> pure . Delayed "" . pure $ dataFunction pure)
  , ("iota" , dataFunction $ \(ComputedValue (LRat x)) -> pure . ComputedValue . LList . fmap (ComputedValue . LRat) $ [0..x])
  , ("?", dataFunction $ \case
    ComputedValue (LRat x) -> 
      pureRat . realToFrac 
      $ unsafePerformIO (uniformRM (0, fromRational x :: Double) =<< createSystemRandom)
    ComputedValue (LList [ComputedValue (LRat a), ComputedValue (LRat b)]) -> 
      pureRat . realToFrac 
      $ unsafePerformIO (uniformRM (fromRational a, fromRational b :: Double) =<< createSystemRandom)
    _ -> throwError "Cannot use function as value")
  , ("::", rankPolymorphicBinary $ \x y -> pure . ComputedValue . LList $ [ComputedValue (LRat x), ComputedValue (LRat y)])
  , (":" , dataFunction $ \x -> pure . dataFunction $ \y -> concatValues x y)
  , ("numerator", dataFunction $ onMultiArray $ pure . fmap ((%1).numerator))
  , ("denominator", dataFunction $ onMultiArray $ pure . fmap ((%1).denominator))

  , ("map", dataFunction $ \f -> pure . dataFunction $ \x'@(ComputedValue x) ->
    case x of
      LList xs -> ComputedValue . LList <$> traverse (executeAsDataFunction f) xs
      _ -> executeAsDataFunction f x')

  , ("keep", dataFunction $ \f -> pure . dataFunction $ \x'@(ComputedValue x) ->
    ComputedValue <$> case x of
      LRat _ -> do
        y <- executeAsDataFunction f x'
        let ComputedValue (LRat p) = y
        pure . LList $ [x' | p > 0]
      LList xs -> do
        ps <- traverse (executeAsDataFunction f) xs
        pure . LList $ [v | (v, ComputedValue (LRat p)) <- Data.List.zip xs ps, p > 0])

  , ("indices", dataFunction $ \f -> pure . dataFunction $ \x'@(ComputedValue x) ->
    ComputedValue <$> case x of
      LRat _ -> executeAsDataFunction f x'
        >>= \case
          ComputedValue (LRat p) -> pure . LList $ [ComputedValue (LRat 0) | p > 0]
          _ -> throwError "Cannot use function or array as an index" 
      LList xs -> do
        ps <- traverse (executeAsDataFunction f) xs
        pure . LList $ [ComputedValue (LRat i) | (i, ComputedValue (LRat p)) <- Data.List.zip [0..] ps, p > 0])

  , ("fold", dataFunction $ \f -> pure . dataFunction $ \(ComputedValue x) ->
    case x of
      LRat v -> pure . ComputedValue . LRat $ v
      LList xs -> Data.List.foldl' (\a b -> do
        g <- executeAsDataFunction f =<< a
        executeAsDataFunction g b) (pure $ Data.List.head xs) (Data.List.tail xs))

  , ("scan", dataFunction $ \f -> pure . dataFunction $ \x'@(ComputedValue x) ->
    ComputedValue <$> case x of
      LRat _ -> pure . LList $ [x']
      LList xs -> fmap LList . sequence $ Data.List.scanl' (\a b -> do
        g <- executeAsDataFunction f =<< a
        executeAsDataFunction g b) (pure $ Data.List.head xs) (Data.List.tail xs))
  
  , ("outer", dataFunction $ \f' -> 
       pure . dataFunction $ \a' -> pure . dataFunction $ \b' -> do 
          a <- rvToMa a' ; b <- rvToMa b'
          let f x y = f' `executeAsDataFunction` ComputedValue (LRat x) 
                 >>= flip executeAsDataFunction (ComputedValue (LRat y))
                 >>= rvToMa
          z <- sequenceA $ f <$> a <*> b
          pure . maToRv . foldMultiArray $ z)
  
  , ("transpose", dataFunction . onMultiArray $ \x ->
    let asList (Single _) = throwError $ pack ("Cannot transpose 1D-Array" #Error)
        asList (Many xs)  = pure xs
    in case x of
      Single v -> pure (Single v)
      Many xs -> fmap (Many . fmap Many . Data.List.transpose) . mapM asList $ xs)
  
  , ("reshape", dataFunction $ \shape' -> pure . dataFunction . onMultiArray $ \x ->
      let isRat (ComputedValue (LRat _)) = True ; isRat _ = False
          asRat (ComputedValue (LRat r)) = r    ; asRat _ = undefined
      in case shape' of
        ComputedValue (LRat (-1)) -> pure . Many . fmap Single . Data.Foldable.toList $ x
        ComputedValue (LRat 0) -> pure . Many $ []
        ComputedValue (LRat p) -> let
            l = Data.Foldable.toList x
          in if Prelude.length l == fromEnum p
            then pure . Many . fmap Single $ l
            else throwError $ pack ("Incorrect dimensions for reshape" #Error) 
        ComputedValue (LList shape'')
          | Prelude.all isRat shape'' -> let
            shape = fromEnum . asRat <$> shape''
            l = Data.Foldable.toList x
            -- TODO(Maxime): -1 dims for greedy takes
            fill [] = pure x
            fill [n] = gets (Many . fmap Single . Prelude.take n) <* modify (Prelude.drop n)
            fill (n:ns) = Many <$> replicateM n (fill ns)
          in evalStateT (fill shape) l
        _ -> throwError $ pack ("Cannot use function as shape" #Error))

  , ("head", dataFunction $ \x'@(ComputedValue x) ->
    case x of
      LRat _ -> pure x'
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure (head xs))
  , ("last", dataFunction $ \x'@(ComputedValue x) ->
    case x of
      LRat _ -> pure x'
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure (Data.List.last xs))

  , ("tail", dataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case x of
      LRat _ -> pure . LList $ []
      LList [] -> throwError $ pack ("NotEnoughRatsError" #Error)
      LList xs -> pure . LList . tail $ xs)

  , ("take", dataFunction $ \(ComputedValue n') -> pure . dataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case (n', x) of
      (LRat _, LRat _) -> pure x
      (LRat n, LList xs) -> pure . LList $ Data.List.take (fromEnum n) xs
      (_, _) -> throwError $ pack ("Bad arguments" #Error))

  , ("rotate", dataFunction $ \(ComputedValue n') -> pure . dataFunction $ \(ComputedValue x) ->
    ComputedValue <$> case (n', x) of
      (LRat n, LList xs) -> pure . LList $ rotate (fromEnum n) xs
                            where rotate :: Int -> [a] -> [a]
                                  rotate a l = Data.List.zipWith const (Data.List.drop a (cycle l)) l
      (_, _) -> throwError $ pack ("Bad arguments" #Error))

  , ("rev", dataFunction $ \x ->
    case x of
      ComputedValue (LRat _) -> pure x
      ComputedValue (LList xs) -> pure . ComputedValue . LList . Data.List.reverse $ xs
      _ -> throwError $ pack ("Cannot reverse function" #Error))

  , ("flat", dataFunction $ let 
        flat = pure . Many . Prelude.foldr ((:).Single) []
      in onMultiArray flat)

  -- TODO: x ∈ xs

  -- TODO(Maxime): implement with onMultiArray
  -- FIXME(Maxime): use throwError
  , ("nth", dataFunction $ \n' -> pure . dataFunction $ \x ->
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
      
  , ("sort", dataFunction $ \x'@(ComputedValue x) -> 
    case x of
      LRat _ -> pure x'
      LList xs -> pure . ComputedValue . LList $ sortOn (\(ComputedValue (LRat v)) -> v) xs)
  , ("nub", dataFunction . onMultiArray $
      let isMany (Many _) = True ; isMany _ = False
          nub' (Single v) = pure (Single v)
          nub' (Many xs)
            | Prelude.null xs = pure $ Many []
            | isMany (Prelude.head xs) = Many <$> traverse nub' xs
            | otherwise = pure . Many $ nub xs
        in nub')
  , ("iter", dataFunction $ \(ComputedValue (LRat n)) -> 
      pure . dataFunction $ \f -> 
      pure . dataFunction $ \x'@(ComputedValue _) ->
        fmap (ComputedValue . LList) . sequence $ Data.List.take (fromEnum n) (iterate (executeAsDataFunction f =<<) $ pure x'))
  ]
  where pureRat = pure . ComputedValue . LRat . continuousApprox
        limit = 1_000_000_000_000_000 
        continuousApprox x
          | denominator x > limit =
            let f = denominator x `div` limit
            in (numerator   x `div` f)
            %  (denominator x `div` f)
          | otherwise = x
        {-
        approx' :: Int -> Rational -> Rational 
        approx' n x = 
          let w = floor x % 1
              f = x - w
          in if f == 0 || n == 0 then w
             else w + 1 / approx' (n-1) (1/f)
        -}

-- NOTE(Maxime): this is actually needed for some reason
builtinNames :: [Text]
builtinNames
  =  -- Combinators
  ["I", "K", "C", "D", "B", "M"]
  ++ -- Numbers
  ["+", "-", "*", "/", "pow"] ++ ["numerator", "denominator"]
  ++ ["?"]
  ++ -- Comparison
  ["=", "!=", ">", ">=", "<", "<="] ++ ["if"]
  ++ -- Folds, unfolds, maps
  ["map", "fold", "scan", "iter", "head", "last", "tail", "take" 
  ,"rotate", "rev", "transpose", "flat", "nub", "indices", "reshape"]
  ++ -- misc ?
  ["nth", "keep", "sort"]
  ++ -- Arrays
  ["iota", ":", "::", "outer"]

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
    LAbs t body -> pure (Delayed t body)
    LApp f' x' ->
      f' >>= \case
        Delayed "" df -> df
        Delayed t df -> executeDelayed t df =<< x'
        ComputedValue v -> (pure.ComputedValue) v

preRender :: MultiArray Rational -> MultiArray String
preRender (Single v)
  | denominator v == 1 = Single $ show (numerator v) #Literal
  | denominator v * numerator v > 1_000_000 =
    Single $  showFFloat (Just 3) (fromRational v :: Float) "" #Literal 
  | otherwise          = Single $ show (numerator v) #Literal <> "/" #Operator <> show (denominator v) #Literal
preRender (Many (Single v:vs))
  | v == stringConstant = Single . (#Literal) $ "\"" 
    <> (toEnum.fromEnum . toSingle <$> vs)
    <> "\""
    where
      toSingle (Single a) = a; toSingle _ = undefined
preRender (Many xs) = Many $ preRender <$> xs

homogeniseDepth :: MultiArray a -> MultiArray a
homogeniseDepth (Single v) = Single v
homogeniseDepth (Many []) = Many []
homogeniseDepth (Many xs)
  | Prelude.all isMany xs = Many $ homogeniseDepth <$> xs 
  | Prelude.any isMany xs = Many $ homogeniseDepth . manyfy <$> xs 
  | otherwise = Many xs
  where
    isMany (Many _) = True ; isMany _ = False
    manyfy (Single v) = Many [Single v] ; manyfy v = v

showVal :: Int -> RuntimeVal m -> String
showVal size' x = case rvToMa x of
  Left _ -> "Cannot show Data Function" #Error
  Right m -> render . homogeniseDepth . preRender $ m
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
    shorten c xs
      | Prelude.length xs <= size' = xs
      | otherwise = Prelude.take (size' - 1) xs ++ [c]

    render (Single v) = v
    render v@(Many xs)
      | depth v == 1 = let
          els  = shorten "…" $ render <$> xs
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
          mat' = fmap (shorten "… " . pad . fmap ((++ " ").render)) $ toList' <$> xs
          mat = shorten (Prelude.replicate (min (Prelude.length (head mat')) size' - 1) "⋮ " ++ ["⋱ "]) mat'
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

-- TODO(Maxime): pretty matrices too ??
valString :: RuntimeVal m -> String
valString (ComputedValue (LRat x)) = pure (toEnum $ fromEnum x)
valString (ComputedValue (LList xs))
  | Prelude.null xs = "[]" #Parens
  | isList (Prelude.head xs) = "[" #Parens <>  Data.List.intercalate " ; " (valString <$> xs) <> "]" #Parens
  | otherwise = Prelude.concatMap valString xs
  where isList (ComputedValue (LList _)) = True ; isList _ = False
valString Delayed{} = "Cannot show Data Function" #Error

renderAsCsv :: RuntimeVal m -> Maybe String
renderAsCsv x = render . homogeniseDepth . preRender' <$> rightToMaybe (rvToMa x)
  where
    rightToMaybe (Right a) = Just a ; rightToMaybe _ = Nothing
    toSingle (Single a) = a; toSingle _ = undefined
    preRender' (Single v) = Single $ show (fromRational v :: Double)
    preRender' (Many (Single v:vs))
      | v == stringConstant = Single $
        toEnum.fromEnum . toSingle <$> vs
    preRender' (Many xs) = Many $ preRender' <$> xs
    render (Single v) = v
    render (Many xs@(Many _:_)) = Data.List.intercalate "\n" (render <$> xs)
    render (Many xs) = Data.List.intercalate "," (render <$> xs)
