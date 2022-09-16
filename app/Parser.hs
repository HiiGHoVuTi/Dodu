module Parser (
  programParser, exprParser
) where

import Data.List
import Data.Ratio
import Data.Text hiding (foldl1', length)
import Lambda
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token

type Program = [(Text, LambdaExpr)]

-- LANGUAGE
glyph :: Parser Char
glyph = noneOf $ " (){}[]"++['0'..'9']

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
programParser = many declParser

-- EXPRESSIONS
num :: Parser Rational
num = (%1) . read <$> many1 digit

array :: Parser [LambdaExpr]
array = char '[' *> exprParser `sepBy` char ','  <* char ']'

litParser :: Parser LambdaExpr 
litParser = fmap (LVal . LRat) num <|> fmap (LVal . LList) array

varParser :: Parser LambdaExpr
varParser = LVar . pack <$> identifier lexer

term, term' :: Parser LambdaExpr
term = litParser <|> varParser <|> trainParser <|> lexer `parens` exprParser
term' = whiteSpace lexer *> term

appParser :: Parser LambdaExpr
appParser = do 
  foldl1' LApp <$> many1 term'

trainParser :: Parser LambdaExpr
trainParser = char '{' *> fmap toTrain (many1 term') <* char '}'
  where
    toTrain o = error $ show (length o) ++ "-trains not yet defined !" 

exprParser :: Parser LambdaExpr
exprParser = appParser <|> term

