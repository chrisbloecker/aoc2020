#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Applicative               ((<|>))
import Control.Monad                     (guard, replicateM_)
import Data.Attoparsec.ByteString        (Parser, (<?>), parseOnly)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
import Data.Maybe                        (isJust)
import Prelude                    hiding (takeWhile)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

data Token = BirthYear      ByteString  -- byr
           | IssueYear      ByteString  -- iyr
           | ExpirationYear ByteString  -- eyr
           | Height         ByteString  -- hgt
           | HairColor      ByteString  -- hcl
           | EyeColor       ByteString  -- ecl
           | PassportID     ByteString  -- pid
           | CountryID      ByteString  -- cid
           | EmptyLine
  deriving (Show)

data Document = Document { byr :: Maybe ByteString
                         , iyr :: Maybe ByteString
                         , eyr :: Maybe ByteString
                         , hgt :: Maybe ByteString
                         , hcl :: Maybe ByteString
                         , ecl :: Maybe ByteString
                         , pid :: Maybe ByteString
                         , cid :: Maybe ByteString
                         }

fromTokens :: Bool -> [Token] -> Document
fromTokens check = fromTokens' Document { byr = Nothing
                                        , iyr = Nothing
                                        , eyr = Nothing
                                        , hgt = Nothing
                                        , hcl = Nothing
                                        , ecl = Nothing
                                        , pid = Nothing
                                        , cid = Nothing
                                        }
  where
    fromTokens' :: Document -> [Token] -> Document
    fromTokens' d []               = d
    fromTokens' d (t@(BirthYear      s):ts) = fromTokens' (d { byr = if check then validate t else Just s }) ts
    fromTokens' d (t@(IssueYear      s):ts) = fromTokens' (d { iyr = if check then validate t else Just s }) ts
    fromTokens' d (t@(ExpirationYear s):ts) = fromTokens' (d { eyr = if check then validate t else Just s }) ts
    fromTokens' d (t@(Height         s):ts) = fromTokens' (d { hgt = if check then validate t else Just s }) ts
    fromTokens' d (t@(HairColor      s):ts) = fromTokens' (d { hcl = if check then validate t else Just s }) ts
    fromTokens' d (t@(EyeColor       s):ts) = fromTokens' (d { ecl = if check then validate t else Just s }) ts
    fromTokens' d (t@(PassportID     s):ts) = fromTokens' (d { pid = if check then validate t else Just s }) ts
    fromTokens' d (t@(CountryID      s):ts) = fromTokens' (d { cid =                               Just s }) ts

validate :: Token -> Maybe ByteString
validate (BirthYear      s) = toMaybe . flip parseOnly s $ decimal >>= \y -> guard (1920 <= y && y <= 2002) >> return s
validate (IssueYear      s) = toMaybe . flip parseOnly s $ decimal >>= \y -> guard (2010 <= y && y <= 2020) >> return s
validate (ExpirationYear s) = toMaybe . flip parseOnly s $ decimal >>= \y -> guard (2020 <= y && y <= 2030) >> return s
validate (Height         s) = toMaybe . flip parseOnly s $ do height <- decimal
                                                              (string "cm" >> guard (150 <= height && height <= 193)) <|> (string "in" >> guard ( 59 <= height && height <=  76))
                                                              endOfInput
                                                              return s
validate (HairColor      s) = toMaybe . flip parseOnly s $ do char '#'
                                                              replicateM_ 6 (satisfy isHexDigit)
                                                              endOfInput
                                                              return s
validate (EyeColor       s) = toMaybe . flip parseOnly s $ do string "amb" <|> string "blu" <|> string "brn" <|> string "gry" <|> string "grn" <|> string "hzl" <|> string "oth"
                                                              endOfInput
                                                              return s
validate (PassportID     s) = toMaybe . flip parseOnly s $ replicateM_ 9 (satisfy isDigit) >> endOfInput >> return s
validate (CountryID      s) = Just s

toMaybe :: Either a b -> Maybe b
toMaybe (Left  _) = Nothing
toMaybe (Right b) = Just b

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f')

--------------------------------------------------------------------------------

parseInput :: ByteString -> Either String [[Token]]
parseInput = parseOnly $ sepBy tokens empty
  where
    isFieldSep :: Char -> Bool
    isFieldSep c = c == ' ' || c == '\n'

    tokens :: Parser [Token]
    tokens = many' (byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid)

    token :: ByteString -> Parser ByteString
    token t = do string t
                 char ':'
                 s <- takeWhile (not . isFieldSep)
                 empty
                 return s

    byr, iyr, eyr, hgt, hcl, ecl, pid, cid, empty :: Parser Token
    byr = BirthYear      <$> token "byr"
    iyr = IssueYear      <$> token "iyr"
    eyr = ExpirationYear <$> token "eyr"
    hgt = Height         <$> token "hgt"
    hcl = HairColor      <$> token "hcl"
    ecl = EyeColor       <$> token "ecl"
    pid = PassportID     <$> token "pid"
    cid = CountryID      <$> token "cid"
    empty = satisfy isFieldSep >> return EmptyLine

--------------------------------------------------------------------------------

isValid :: Document -> Bool
isValid Document{..} =
  all isJust [byr, iyr, eyr, hgt, hcl, ecl, pid]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  mtokens <- parseInput <$> BS.readFile "input.txt"

  case mtokens of
    Left err -> putStrLn err
    Right tokens -> do
      let unvalidated = map (fromTokens False) tokens
          validated   = map (fromTokens True)  tokens

      putStr "(1) Number of passports:       "
      print . length . filter isValid $ unvalidated

      putStr "(2) Number of valid passports: "
      print . length . filter isValid $ validated
