#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                (void)
import Data.List                    ((\\), intersect, union, elem, sort)
import Data.Text                    (Text, intercalate, unpack)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

type Book       = [Recipe]
type Ingredient = Text
type Allergen   = Text

data Recipe = Recipe { ingredients :: [Text]
                     , allergens   :: [Text]
                     }

instance Show Recipe where
  show Recipe{..} = unpack
                  $ mconcat [ intercalate " " ingredients
                            , "(contains "
                            , intercalate ", " allergens
                            , ")"
                            ]

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

bookP:: Parser Book
bookP = many (do ingredients <- some (takeWhile1P (Just "ingredient") isChar <* char ' ') <?> "ingredients"
                 string "(contains "
                 allergens   <- some (takeWhile1P (Just "allergen")   isChar <* choice [ void (string ", ")
                                                                                       , return ()
                                                                                       ]) <?> "allergens"
                 char ')'
                 eol
                 return Recipe{..}
             ) <* eof
  where
    isChar c = 'a' <= c && c <= 'z'

--------------------------------------------------------------------------------

matchIngredients :: Book -> [(Allergen, Ingredient)]
matchIngredients book =
  let allAllergens = foldl1 union (map allergens book)
      candidates   = [ (allergen, foldl1 intersect [ ingredients
                                                   | Recipe{..} <- book
                                                   , allergen `elem` allergens
                                                   ]
                       )
                     | allergen <- allAllergens
                     ]
      matched      = [ (allergen, ingredient)
                     | (allergen, [ingredient]) <- candidates
                     ]
  in case allAllergens of
    [] -> []
    _  -> matched ++ matchIngredients [ recipe { ingredients = ingredients \\ map snd matched
                                               , allergens   = allergens   \\ map fst matched
                                               }
                                      | recipe@Recipe{..} <- book
                                      ]

main :: IO ()
main = do
  input <- parse bookP "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right book -> do
      let matched              = matchIngredients book
          dangerousIngredients = map snd matched
      putStrLn $ "(1) " ++ show (length . concatMap ((\\ dangerousIngredients) . ingredients) $ book)
      putStrLn $ "(2) " ++ unpack (intercalate "," . map snd . sort $ matched)
