{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
module Parser (
  programParser, exprParser,
  parseProgram, parseExpr,
  parseCSV
) where

import Data.CSV
import Data.Fix
import Data.List
import Data.Ratio -- Int
import Data.Text hiding (foldl1', length, zipWith, take)
import Lambda
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token

type Program = [(Text, LambdaExpr)]

-- LANGUAGE
glyph :: Parser Char
glyph = noneOf $ " (){}[],;\n\t\\."++['0'..'9']

languageDef :: LanguageDef ()
languageDef = emptyDef
  { commentLine     = "--"
  , commentStart    = "{-"
  , commentEnd      = "-}"
  , caseSensitive   = True
  , identStart      = glyph
  , identLetter     = glyph
  , reservedNames   = ["Dodu", ".", "\\", "@", "|>"]
  , reservedOpNames = ["<-"]
  }

lexer :: TokenParser () 
lexer = makeTokenParser languageDef

-- PROGRAMS
declParser :: Parser (Text, LambdaExpr)
declParser = do
  _ <- whiteSpace lexer
  i <- pack <$> identifier lexer
  _ <- reservedOp lexer "<-"
  e <- exprParser <* char '.'
  _ <- whiteSpace lexer
  return (i, e)

programParser :: Parser Program
programParser = many declParser <* eof

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse programParser

-- EXPRESSIONS
num :: Parser Rational
num =      (%1)    . read <$> many1 digit
  <|> try ((%(-1)) . read <$> (char '-' *> many1 digit))

array :: Parser [LambdaExpr]
array = char '[' *> exprParser `sepBy` (whiteSpace lexer *> char ';')  <* char ']'

litParser :: Parser LambdaExpr 
litParser = fmap sta (stringLiteral lexer) 
        <|> fmap (lVal . LRat) num 
        <|> fmap (lVal . LList) array
  where
    sta :: String -> LambdaExpr
    sta = lVal . LList .
       (lVal (LRat (1%10_000_000_000_000_001)) :) 
      . fmap (lVal . LRat . toEnum . fromEnum)

varParser :: Parser LambdaExpr
varParser = wrapFix . LVar . pack <$> identifier lexer

term, term' :: Parser LambdaExpr
term = litParser <|> varParser <|> trainParser <|> lexer `parens` exprParser
term' = whiteSpace lexer *> term <* whiteSpace lexer

compositionParser :: Parser LambdaExpr
compositionParser = do
  f <- try appParser <|> term'  
  g <- char ',' *> exprParser
  let x = "" -- FIXME(Maxime): get UID 
    in pure $ lAbs x (lApp g (lApp f (lVar x)))

andThenParser :: Parser LambdaExpr
andThenParser = do
  f <- try appParser <|> term'  
  g <- string "|>" *> exprParser
  pure $ lApp g f

appParser :: Parser LambdaExpr
appParser = do
  foldl1' lApp <$> many1 term'

trainParser :: Parser LambdaExpr
trainParser = char '{' *> fmap toTrain (many1 term') <* char '}'
  where
    -- λax.a
    toTrain [a] = lAbs "_" a
    -- λabx.a(bx)x
    toTrain [a, b] = 
      let x = "" -- FIXME(Maxime): UID
      in lAbs x (lApp (lApp a (lApp b (lVar x))) (lVar x))
    -- λabcx.a(bx)(cx)
    toTrain [a, b, c] = 
      let x = "" -- FIXME(Maxime): UID
      in lAbs x (lApp (lApp b (lApp a (lVar x))) (lApp c (lVar x)))
    toTrain o = error $ show (length o) ++ "-trains not yet defined !" 

lambdaParser :: Parser LambdaExpr
lambdaParser = do
  _ <- whiteSpace lexer *> char '\\'
  names <- many1 (identifier lexer)
  _ <- whiteSpace lexer *> char '.'
  body <- exprParser
  pure $ Data.List.foldl' (flip lAbs) body (pack <$> Data.List.reverse names)

letParser :: Parser LambdaExpr
letParser = do
  _ <- whiteSpace lexer *> char '@' <* whiteSpace lexer
  elems <- parens lexer (many1 declParser)
  body <- exprParser
  pure $ Data.List.foldl' doTheLet body elems 
  where
    doTheLet body (name, value) = lApp (lAbs name body) value
  

exprParser :: Parser LambdaExpr
exprParser = try letParser 
         <|> try lambdaParser 
         <|> try andThenParser
         <|> try compositionParser
         <|> try appParser
         <|> term'

parseExpr :: SourceName -> String -> Either ParseError LambdaExpr
parseExpr = parse (exprParser <* eof)

csvParser :: Parser (RuntimeVal m)
csvParser = toVal <$> csvFile
  where
    toVal = ComputedValue . LList 
          . fmap (ComputedValue . LList 
          . fmap (ComputedValue . strToLVal))
    strToLVal x = case parse (fromIntegral <$> try (integer lexer)
                                                <|> float lexer)
                      "" x of
      Right f -> LRat (realToFrac f)
      Left _ -> LList 
        $ ComputedValue (LRat (1 % 10_000_000_000_000_001)) 
        : (ComputedValue .LRat. toEnum.fromEnum <$> x)

parseCSV :: SourceName -> String -> Either ParseError (RuntimeVal m)
parseCSV = parse csvParser

