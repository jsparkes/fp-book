module Ch11
  ( test
  ) where

import Prelude (class Ord, Unit, discard, negate, otherwise, show, type (~>), ($), (>), (+), (<>))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:), singleton)
import Data.NonEmpty (NonEmpty(..), (:|))
-- import Data.NonEmpty (NonEmptyList(..), (:|))
import Data.Maybe (Maybe(..))
import Data.Semiring (class Semiring, add, zero)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  -- log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
  -- log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

-- log $ show $ sum (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
-- log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
-- log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
reverse :: ∀ a. List a -> List a
reverse l = foldl (\acc x -> x : acc) Nil l

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x > y = x -- COMPILER WARNING!!
  | otherwise = y

-- findMax :: ∀ a. Ord a => a -> List a -> a
findMax :: ∀ a. Ord a => List a -> Maybe a
-- findMax d l = foldl (\acc x -> max acc x) d l
findMax Nil = Nothing

-- findMax (x : Nil) = x
findMax (x : xs) = Just $ foldl (\acc x' -> max acc x') x xs

-- findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
-- findMaxNE (x :| xs) = foldl max x xs
-- findMaxNE l = foldl1 max l
foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
-- foldl1 f (NonEmpty x xs) = foldl f x xs
foldl1 f (x :| xs) = foldl f x xs

-- sum :: List Int -> Int
-- sum Nil = 0
-- sum (x : xs) = x + sum xs
-- sum = go 0
--   where
--   go t Nil = t
--   go t (x : xs) = go (t + x) xs
sum :: ∀ a f. Semiring a => Foldable f => f a -> a
sum = foldl add zero

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

-- instance foldableTree :: Foldable a where
--   --   -- foldl f b (f a) a 
--   foldr f acc t = foldr f acc (toList t)
--   foldl f acc t = foldl f acc (toList t)
--   foldMap f acc t = foldMap f acc (toList t)
toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x

toList (Node x y) = toList x <> toList y
