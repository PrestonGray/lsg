module Filters where

import Data.Char (toLower)

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = toLower x == toLower y && prefix xs ys
