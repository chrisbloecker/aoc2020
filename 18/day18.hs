#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                              (void)
import Control.Monad.Combinators.Expr
import Data.Text                                  (Text)
import Data.Void                                  (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Data.Text.IO               as T  (readFile)
import qualified Text.Megaparsec.Char.Lexer as L
--------------------------------------------------------------------------------

data Expr = Mul Expr Expr
          | Add Expr Expr
          | Val Int

instance Show Expr where
  show (Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Val v)     = show v

eval :: Expr -> Int
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2
eval (Val x)     = x

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVal :: Parser Expr
pVal = Val <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pExpr1 :: Parser Expr
pExpr1 = makeExprParser pTerm opTable
  where
    pTerm = choice [ parens pExpr1
                   , pVal
                   ]
    opTable = [ [ binary "*" Mul
                , binary "+" Add
                ]
              ]

pExpr2 :: Parser Expr
pExpr2 = makeExprParser pTerm opTable
  where
    pTerm = choice [ parens pExpr2
                   , pVal
                   ]
    opTable = [ [ binary "+" Add ]
              , [ binary "*" Mul ]
              ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input1 <- parse (many (pExpr1 <* eol) <* eof) "input.txt" <$> T.readFile "input.txt"
  case input1 of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right exprs -> putStrLn $ "(1) " ++ show (sum . map eval $ exprs)

  input2 <- parse (many (pExpr2 <* eol) <* eof) "input.txt" <$> T.readFile "input.txt"
  case input2 of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right exprs -> putStrLn $ "(2) " ++ show (sum . map eval $ exprs)
