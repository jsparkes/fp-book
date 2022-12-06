module Ch7b where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test =
  let
    person =
      Person
        { name: FullName "Sue Smith"
        , age: Age 23
        , occupation: Doctor
        }
  in
    -- log $ show $ toCSV person
    --   == CSV "Sue Smith,23,Doctor"
    log
      $ show
      $ ( toCSV person
            # fromCSV
        )
      == Just person

newtype CSV
  = CSV String

derive instance newtypeCSV :: Newtype CSV _

derive newtype instance eqCSV :: Eq CSV

derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName
  = FullName String

derive instance eqFullName :: Eq FullName

instance showFullName :: Show FullName where
  show (FullName f) = f

newtype Age
  = Age Int

derive instance eqAge :: Eq Age

derive instance newtypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance eqOccupation :: Eq Occupation

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person p) =
    CSV
      -- "name, age, occupation\n" 
      ( show p.name
          <> ","
          <> show p.age
          <> ","
          <> show p.occupation
      )

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCVSPerson :: FromCSV Person where
  fromCSV (CSV csv) = case split (Pattern ",") csv of
    [ name, age, occupation ] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person { name: FullName name, age: Age age', occupation: occupation' }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing
