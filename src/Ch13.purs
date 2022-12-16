module Ch13
  ( test
  ) where

import Prelude (class Eq, class Show, Unit, show, discard, identity, ($), (&&), (/), (<>), (==), (*))
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Threeple 10 20 40
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))

-- log $ show $ rmap (_ * 2) $ (Left "error reason" :: Either _ Unit)
-- log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
-- log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
-- log $ show $ lmap toUpper $ Right 10
-- log $ show $ rmap (_ * 2) $ Tuple 80 40
-- log $ show $ lmap (_ / 2) $ Tuple 80 40
-- log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
data Maybe a
  = Nothing
  | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

-- instance showMaybe :: Show Maybe a where
--   show Nothing = "Nothing"
--   show (Just x) = "Just " <> show x
instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

data Either a b
  = Left a
  | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map f (Right x) = Right $ f x
  map _ (Left x) = Left x

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x $ f y

data Threeple a b c
  = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y $ f z

-- instance bifunctorEither :: Bifunctor (Either) where
--   bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d
--   bimap f _ (Left err) = Left $ f err
--   bimap _ g (Right x) = Right $ g x
-- rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
-- rmap = bimap identity
-- lmap f = bimap f identity
-- instance bifunctorTuple :: Bifunctor Tuple where
--   bimap f g (Tuple x y) = Tuple (f x) (g y)
-- rmap _ g (Tuple x y) = Tuple x (g y)
-- lamp f _ (Tuple x y) = Tuple (f x) y
