module CH11.Language  where

import Data.Char
import Data.List (intercalate, dropWhile, dropWhileEnd)
import Data.List.Split (endBy)

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (c:cs) = (toUpper c) : cs

capitalizeParagraph :: String -> String
capitalizeParagraph ts = intercalate ". " $ map (capitalizeWord . trim)  (endBy "." ts)

trim = dropWhileEnd isSpace . dropWhile isSpace
