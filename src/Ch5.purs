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

import Prelude (Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<<<), (>>>), (<), (>), (>=), (==), type (~>))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  --   log $ show $ flip const 1 2
  --   flip const 1 2 # show # log
  --   log $ show $ singleton "xyz"
  --   log $ show $ null Nil
  --   log $ show $ null ("abc" : Nil)
  --   log $ show $ snoc (1 : 2 : Nil) 3
  --   log $ show $ length $ 1 : 2 : 3 : Nil
  --   log $ show $ (head Nil :: Maybe Unit)
  --   log $ show $ head ("abc" : "123" : Nil)
  --   log $ show $ (tail Nil :: Maybe (List Unit))
  --   log $ show $ (last Nil :: Maybe Unit)
  --   log $ show $ last ("a" : "b" : "c" : Nil)
  --   log $ show $ init (Nil :: List Unit)
  --   log $ show $ init (1 : Nil)
  --   log $ show $ init (1 : 2 : Nil)
  --   log $ show $ init (1 : 2 : 3 : Nil)
  --   log $ show $ uncons (1 : 2 : 3 : Nil)
  --   log $ show $ index (1 : Nil) 4
  --   log $ show $ index (1 : 2 : 3 : Nil) 1
  --   log $ show $ index (Nil :: List Unit) 0
  --   log $ show $ index (1 : 2 : 3 : Nil) (-99)
  --   log $ show $ (1 : 2 : 3 : Nil) !! 1
  --   log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  --   log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  --   log $ show $ findIndex (10 /= _) (Nil :: List Int)
  --   log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  --   log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  --   log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  --   log $ show $ reverse (10 : 20 : 30 : Nil)
  --   log $ show
  --     $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show
    $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))

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

filter :: forall a. (a -> Boolean) -> List a -> List a
-- filter f Nil = Nil
-- filter f (x : xs) = if f x then x : filter f xs else filter f xs
filter pred l = go Nil l # reverse
  where
  go result Nil = result

  go result (x : xs) = if pred x then go (x : result) xs else go result xs

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes l = go l Nil # reverse
  where
  go Nil result = result

  go (x : xs) result = case x of
    Nothing -> go xs result
    Just y -> go xs (y : result)

range :: Int -> Int -> List Int
-- range start end
--   | start == end = singleton end
--   | start < end = start : range (start + 1) end
--   | true = start : range (start - 1) end
range start end = go end (Nil :: List Int)
  where
  go :: Int -> List Int -> List Int
  go e result
    | start == e = result
    | start < e = go (e - 1) (e : result)
    | otherwise = go (e + 1) (e : result)

take :: forall a. Int -> List a -> List a
-- take _ Nil = Nil
-- take 0 _ = Nil
-- take n (x : xs) = x : take (n - 1) xs
take n l = go (max n 0) l Nil # reverse
  where
  go _ Nil result = result

  go 0 _ result = result

  go n' (x : xs) result = go (n' - 1) xs (x : result)

drop :: ∀ a. Int -> List a -> List a
drop 0 xs = xs

drop _ Nil = Nil

drop n (_ : xs) = drop (n - 1) xs

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil

takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil

dropWhile f (x : xs) = if f x then dropWhile f xs else x : xs

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil

  go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl

dropEnd :: forall a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil

  go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil

zip _ Nil = Nil

zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip l = go l (Tuple Nil Nil)
  where
  go Nil result = result

  go ((Tuple a b) : xs) (Tuple x y) = go xs $ Tuple (a : x) (b : y)
