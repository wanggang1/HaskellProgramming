module DBItems where

import Data.Time
import Data.Typeable

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show, Typeable)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello world!"
  , DbNumber 1
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 2
  , DbDate (UTCTime (fromGregorian 1931 5 1) (secondsToDiffTime 34123))
  , DbDate (UTCTime (fromGregorian 1901 5 1) (secondsToDiffTime 34123))
  , DbNumber 3
 ]

filterDates :: [DatabaseItem] -> [UTCTime]
filterDates = foldr (\x b -> getDate x ++ b) []

filterNumbers :: [DatabaseItem] -> [Integer]
filterNumbers = foldr (\x b -> getNumber x ++ b) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr (\x b -> if x > b then x else b) (head ts) (tail ts)
                where ts = filterDates xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterNumbers xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral(sumDb xs) / (fromIntegral $ length xs)

getDate :: DatabaseItem -> [UTCTime]
getDate (DbDate utcTime) = [utcTime]
getDate _ = []

getNumber :: DatabaseItem -> [Integer]
getNumber (DbNumber n) = [n]
getNumber _ = []
