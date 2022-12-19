module Ch15
  ( even
  , odd
  , test
  ) where

import Prelude
import Control.Alt (class Alt)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String.CodePoints (length)
import Data.Traversable (class Traversable, sequence, traverse)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [ 1, 2, 3 ]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

newtype Predicate a
  = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

data Moore s a b
  = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap f g (Moore s0 output transition) = Moore s0 (g <<< output) (\s -> transition s <<< f)

-- Moore s a b
addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => (Moore s a b) -> f a -> b
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

sizer :: Moore Int String String
-- sizer xs = Moore 0 (\len -> "Length is " <> show len) (\l s -> l + length s)
sizer = dimap length (\len -> "Size is " <> show len) addr

-- instance traversableList :: Traversable List where
--   traverse :: ∀ a b m. Applicative m => (a -> m b) -> List a -> m (List b)
--   traverse f = foldr (\x acc -> (:) <$> f x <*> acc) (pure Nil)
--   sequence :: ∀ a m. Applicative m => List (m a) -> m (List a)
--   sequence = traverse identity
-- instance altList :: Alt List where
--   alt = (<>)
-- instance altMaybe :: Alt Maybe where
--   alt Nothing r = r
--   alt l _ = l
-- instance altEither :: Alt (Either a) where
--   alt (Left _) r = r
--   alt l _ = l
