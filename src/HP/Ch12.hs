module HP.Ch12 where

chapter12 :: IO ()
chapter12 = putStrLn "Chapter 12 - Signaling Adversity"

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

ageOkey :: Age -> Either [PersonInvalid] Age
ageOkey age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkey :: Name -> Either [PersonInvalid] Name
nameOkey name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson' :: Name -> Age -> ValidatePerson Person
mkPerson' name age = mkPersonV (nameOkey name) (ageOkey age)

mkPersonV :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPersonV (Right name) (Right age) = Right (Person name age)
mkPersonV (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPersonV (Left badName) _ = Left badName
mkPersonV _ (Left badAge) = Left badAge
