{-# LANGUAGE OverloadedStrings #-}
module Parser (
  programParser, exprParser,
  parseProgram, parseExpr
) where

import Data.Fix
import Data.List
import Data.Ratio
import Data.Text hiding (foldl1', length, zipWith, take)
import Lambda
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token

type Program = [(Text, LambdaExpr)]

-- LANGUAGE
glyph :: Parser Char
glyph = noneOf $ " (){}[],;"++['0'..'9']

languageDef :: LanguageDef ()
languageDef = emptyDef
  { commentLine     = "--"
  , commentStart    = "{-"
  , commentEnd      = "-}"
  , caseSensitive   = True
  , identStart      = glyph
  , identLetter     = glyph
  , reservedNames   = ["Dodu"]
  , reservedOpNames = ["<-"]
  }

lexer :: TokenParser () 
lexer = makeTokenParser languageDef

-- PROGRAMS
declParser :: Parser (Text, LambdaExpr)
declParser = do
  i <- pack <$> identifier lexer
  _ <- reservedOp lexer "<-"
  e <- exprParser
  return (i, e)

programParser :: Parser Program
programParser = many declParser <* eof

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse programParser

-- EXPRESSIONS
num :: Parser Rational
num = (%1) . read <$> many1 digit

array :: Parser [LambdaExpr]
array = char '[' *> exprParser `sepBy` (whiteSpace lexer *> char ';')  <* char ']'

litParser :: Parser LambdaExpr 
litParser = fmap (lVal . LRat) num <|> fmap (lVal . LList) array

varParser :: Parser LambdaExpr
varParser = wrapFix . LVar . pack <$> identifier lexer

term, term' :: Parser LambdaExpr
term = litParser <|> varParser <|> trainParser <|> lexer `parens` exprParser
term' = whiteSpace lexer *> term <* whiteSpace lexer

compositionParser :: Parser LambdaExpr
compositionParser = do
  f <- term'
  _ <- char ','
  g <- term'
  let x = "" -- FIXME(Maxime): get UID 
    in pure $ lAbs x (lApp g (lApp f (lVar x)))

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

exprParser :: Parser LambdaExpr
exprParser = try compositionParser <|> try appParser <|> term'

parseExpr :: SourceName -> String -> Either ParseError LambdaExpr
parseExpr = parse exprParser

