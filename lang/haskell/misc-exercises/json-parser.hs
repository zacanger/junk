#!/usr/bin/env runhaskell

-- ./json-parser.hs '{ "foo": 2, "bar": "whatever" }'

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Monad
import Data.Char
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ map (parse parser "") args

data JValue = JObject [(JKey,JValue)]
          | JList [JValue]
          | JSingle JKey
    deriving (Read,Show)

data JKey = JKeyString String
  | JKeyNum JNum
  | JKeyBool Bool
  | JKeyNull
  deriving (Read,Show)

data JNum = JNumInt Int | JNumFraction Double
 deriving (Read,Show)

spaceOut p = between (many space) (many space) p

parseNull = string "null" >> return JKeyNull

parseKey = choice (map try [parseBool,parseString,parseNum,parseNull])

parseSingle = fmap JSingle parseKey

parseBool = do
  b <- (string "true" <|> string "false")
  return (JKeyBool (b == "true"))

parseString = do
  char '"'
  str <- manyTill (parseEscapeChar <|> anyChar) (char '"')
  return (JKeyString str)

parseEscapeChar = do
  char '\\'
  eitherEscapeChar <- (fmap Left (char 'u' >> parseUnicodePointCode)) <|> (fmap Right (parseAsciiEscapeKey))
  let escapeSequence = case eitherEscapeChar of
        Left hexCode -> "\\x"++hexCode
        Right charKey -> "\\"++[charKey]
  return (read ("'"++escapeSequence++"'") :: Char)

parseAsciiEscapeKey = oneOf "\\/bfnrt"
parseUnicodePointCode = replicateM 4 (satisfy isHexDigit)

parseKeyValuePair = do
  k <- spaceOut parseKey
  char ':'
  o <- parseValue
  return (k,o)

parseObject = do
  char '{'
  pairs <- sepBy parseKeyValuePair (char ',')
  char '}'
  return (JObject pairs)

parseList = do
  char '['
  values <- sepBy parseValue (char ',')
  char ']'
  return (JList values)

parseValue = choice (map (try . spaceOut) [parseObject,parseList,parseSingle])

parser = do
  b <- parseValue
  eof
  return b

parseSign = char '-'
parseNatChars = many1 (oneOf "0123456789")
parseFractionalPart = char '.' >> parseNatChars
parseExponentPart = do
  oneOf "eE"
  sign <- many parseSign
  str  <- parseNatChars
  return (read (sign ++ str) :: Double)

caseMaybe m f a = case m of
 (Just b) -> f a b
 _        -> a

raise n e    = n * (10**e)
raiseInt n e = n * (10^(round e))

parseNum = do
  sign     <- fmap maybeToList $ optionMaybe parseSign
  natpart  <- parseNatChars
  fracpart <- optionMaybe parseFractionalPart
  expo     <- optionMaybe parseExponentPart
  let isFractional = maybe False (<0) expo || isJust fracpart
  let fracpart'    = fromMaybe "0" fracpart
  return $ JKeyNum $ case isFractional of
    True -> JNumFraction
        $ caseMaybe expo raise
        $ (read (sign++natpart++"."++fracpart'):: Double)
    False -> JNumInt
        $ caseMaybe expo raiseInt
        $ (read (sign++natpart):: Int)
