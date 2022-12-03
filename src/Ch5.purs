module Ch5
  ( (#)
  , ($)
  , apply
  , applyFlipped
  , const
  , flip
  , head
  , init
  , last
  , length
  , null
  , singleton
  , snoc
  , tail
  , test
  , uncons
  ) where

import Prelude (Unit, discard, negate, show, (+), (-), (/=), (<), (>=), (==), type (~>))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  --   log $ show $ flip const 1 2
  --   flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ (tail Nil :: Maybe (List Unit))
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show
    $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)

-- log $ show $ (tail "abc" : "123" : Nil)
const :: ∀ a b. a -> b -> a
const x _ = x

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
-- applyFlipped = x f = f x
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true

null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = x : Nil

snoc (xs : ys) x = xs : snoc ys x

length :: forall a. List a -> Int
-- length Nil = 0
-- length (x : xs) = 1 + (length xs)
length xs = length' 0 xs
  where
  length' :: ∀ b. Int -> List b -> Int
  length' acc Nil = acc

  length' acc (_ : ys) = length' (acc + 1) ys

head :: ∀ a. List a -> Maybe a
head Nil = Nothing

head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing

tail (_ : x) = Just x

last :: forall a. List a -> Maybe a
last Nil = Nothing

last (x : Nil) = Just x

last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing

init (_ : Nil) = Nothing

init l = Just $ go l
  where
  go Nil = Nil

  go (_ : Nil) = Nil

  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing

-- uncons (x : Nil) = { head: x, tail: Nil }
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing

index _ i
  | i < 0 = Nothing

index (x : _) 0 = Just x

index (_ : xs) pos = index xs (pos - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex f l = go l 0
  where
  go Nil _ = Nothing

  go (x : xs) i = if f x then Just i else go xs (i + 1)

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex f l = go l 0 Nothing
  where
  go :: List a -> Int -> Maybe Int -> Maybe Int
  go Nil i _ = Just i

  go (x : xs) i v = go xs (i + 1) (if f x then Just i else v)

reverse :: List ~> List
reverse l = go l Nil
  where
  go Nil l' = l'

  go (x : xs) l' = go xs (x : l')

concat :: forall a. List (List a) -> List a
concat Nil = Nil

concat (Nil : xss) = concat xss

concat ((x : xs) : xss) = x : concat (xs : xss)
