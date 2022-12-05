module Ch6 where

import Prelude
import Data.Array (sort)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Place
  = First
  | Second
  | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordPlace :: Ord Place where
  compare First First = EQ
  compare First _ = LT
  compare Second Second = EQ
  compare Second First = GT
  compare Second Third = LT
  compare Third Third = EQ
  compare Third _ = GT

x :: Array Place
x = [ Third, First, Second ]

sx :: Array Place
sx = sort x

instance showPlace :: Show Place where
  show First = "First"
  show Second = "Second"
  show Third = "Third"

data SomeType
  = This
  | That
  | TheOther
  | AndYetAnother

derive instance eqSomeType :: Eq SomeType

derive instance ordSomeType :: Ord SomeType

-- derive instance genericSomeTypeGeneric :: Generic SomeType
-- instance showSomeType :: Show SomeType where
--   show = genericShow
