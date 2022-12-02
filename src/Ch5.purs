module Ch5 where

import Prelude (Unit, discard, show)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log

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
