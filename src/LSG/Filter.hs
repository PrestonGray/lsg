module LSG.Filter where

import Data.Char (toLower)

-- | Checks if the search string exists as a substring in the target string
substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

-- | Checks if the search string matches the beginning of the target string
prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = toLower x == toLower y && prefix xs ys
