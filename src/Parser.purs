module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons, fromCharArray, singleton)
import Data.String.CodePoints (codePointFromChar)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
  log $ show $ parse' yearFirst "1999-12-31"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"

type ParserState a = Tuple String a

-- data ParserState a = ParserState String a
-- newtype ParserState a = ParserState (Tuple String a)

class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

instance parserFunctor :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  map f p = Parser \s -> map f <$> parse p s

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  -- apply p1 p2 = Parser \s -> case parse p1 s of
  --   Left err -> Left err
  --   Right (Tuple s1 h) -> case parse p2 s1 of
  --     Left err -> Left err
  --     Right (Tuple s2 x) -> Right (Tuple s2 (h x))
  -- apply = ap
  apply p1 p2 = Parser \s -> do
    Tuple s1 f <- parse p1 s
    Tuple s2 x <- parse p2 s1
    pure $ Tuple s2 (f x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> Right $ Tuple s x

parse :: ∀ e a. ParserError e => Parser e a -> ParseFunction e a
parse (Parser f) = f

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [ c1, c2, c3 ]) <$> char <*> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [ c1, c2, c3 ]

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = do
  c <- char
  if (pred c) then
    pure c
  else
    fail $ invalidChar expected

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

count :: forall a e f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

-- Monadic parser parts
instance bindParser :: Bind (Parser e) where
  bind p f = Parser \s -> do
    Tuple s1 x <- parse p s
    parse (f x) s1

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "ASCII letter" (isAlpha <<< codePointFromChar)

instance altParser :: Alt (Parser e) where
  alt p1 p2 = Parser \s ->
    case parse p1 s of
      Left _ -> parse p2 s
      Right x -> Right x

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = digit <|> letter <|> fail (invalidChar "alphaNum")

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

data DateFormat = YearFirst | MonthFirst

derive instance genericDateFormat :: Generic DateFormat _

instance showDateFormat :: Show DateFormat where
  show = genericShow

newtype Year = Year Int

derive instance genericYear :: Generic Year _

instance showYear :: Show Year where
  show = genericShow

newtype Month = Month Int

derive instance genericMonth :: Generic Month _

instance showMonth :: Show Month where
  show = genericShow

newtype Day = Day Int

derive instance genericDay :: Generic Day _

instance showDay :: Show Day where
  show = genericShow

type DateParts =
  { year :: Year
  , month :: Month
  , day :: Day
  , format :: DateFormat
  }

-- derive instance genericDateParts :: Generic DateParts _

-- instance showDateParts :: Show DateParts where
--   show = genericShow

-- atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
-- atMost n p
--   | n <= 0 = pure []
--   | otherwise = optional [] $ p >>= \c -> (c : _) <$> atMost (n - 1) p

atMost :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

-- range :: ∀ e a. Int -> Int -> Parser e a -> Parser e (Array a)
-- range min max p
--   | min < 0 || max <= 0 || max < min = pure []
--   | otherwise = count min p >>= \cs -> (cs <> _) <$> atMost (:) (max - min) p

range
  :: ∀ e a f
   . Traversable f
  => Unfoldable f
  => Semigroup (f a)
  => (a -> f a -> f a)
  -> Int
  -> Int
  -> Parser e a
  -> Parser e (f a)
range cons min max p
  | min < 0 || max <= 0 || max < min = pure none
  | otherwise = count min p -- COMPILER ERROR!!

      >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count' 4 digit
  -- year <- count' 4 digit # fromString # fromMaybe 0 # Year
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< digitsToNum <$> range' 1 2 digit

  pure { year, month, day, format: YearFirst }

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure { year, month, day, format: MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

instance lazyParser :: Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse (f unit) s

some
  :: ∀ a f m
   . Unfoldable f
  => Alt m
  => Applicative m
  => Lazy (m (f a))
  => (a -> f a -> f a)
  -> m a
  -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p -- COMPILER ERROR!!

many
  :: ∀ a f m
   . Unfoldable f
  => Alt m
  => Applicative m
  => Lazy (m (f a))
  => (a -> f a -> f a)
  -> m a
  -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  cg1 <- range' 1 4 digit
  constChar ','
  constChar ' '
  cg2 <- some' (letter <|> constChar' ' ')
  cg3 <- many' digit
  pure [ cg1, cg2, cg3 ]
