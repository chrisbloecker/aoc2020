#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Map                          (Map, (!))
import Data.Text                         (Text, unpack)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO      as T  (readFile)
import qualified Data.Map          as M  (fromList, insert)
--------------------------------------------------------------------------------

data Rule = Seq [Int]
          | Alt Rule Rule
          | Tok Char
  deriving (Show)

type Pattern = [Rule]

matches :: Map Int Rule -> String -> Pattern -> Bool
matches _       ""  []           = True
matches rules    s  (Seq rs :ps) =           matches rules s ([rules ! r | r <- rs] ++ ps)
matches rules    s  (Alt l r:ps) =           matches rules s (l:ps)
                                ||           matches rules s (r:ps)
matches rules (c:s) (Tok t  :ps) = c == t && matches rules s ps
matches _        _  _            = False

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

inputP :: Parser (Map Int Rule, [String])
inputP = (,) <$> fmap M.fromList (many ruleP <* eol)
             <*> (many messageP <* eof)

ruleP :: Parser (Int, Rule)
ruleP = do ruleID <- decimal <?> "ruleP ruleID"
           string ": "
           rule <- choice [ try altP, seqP, tokP ] <?> "ruleP rule"
           eol
           return (ruleID, rule)

  where
    seqP :: Parser Rule
    seqP = do r1 <- decimal <?> "seqP r1"
              choice [ try $ do char ' '
                                r2 <- decimal <?> "seqP r2"
                                return (Seq [r1, r2])
                     , return (Seq [r1])
                     ]

    altP :: Parser Rule
    altP = do r1 <- seqP <?> "altP r1"
              string " | "
              r2 <- seqP <?> "altP r2"
              return (Alt r1 r2)

    tokP :: Parser Rule
    tokP = do char '"'
              c <- choice [ char 'a', char 'b' ] <?> "tokP c"
              char '"'
              return (Tok c)

messageP :: Parser String
messageP = unpack <$> takeWhile1P (Just "message") (\c -> c == 'a' || c == 'b') <* eol

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input1 <- parse inputP "input.txt" <$> T.readFile "input.txt"
  case input1 of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right (rules, messages) -> do
      putStrLn $ "(1) " ++ show (length . filter id . map (\m -> matches rules m [Seq [0]]) $ messages)

      let rules' = M.insert 11 (Alt (Seq [42, 31]) (Seq [42, 11, 31]))
                 . M.insert  8 (Alt (Seq [42    ]) (Seq [42, 8     ]))
                 $ rules
      putStrLn $ "(2) " ++ show (length . filter id . map (\m -> matches rules' m [Seq [0]]) $ messages)
