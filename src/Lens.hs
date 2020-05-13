module Lens where

import Data.Bifunctor

data Prism s t a b = Prism { build :: b -> t, match :: s -> Either t a }

_Left :: Prism (Either a x) (Either b x) a b
_Left = Prism Left (either Right (Left . Right))

_Right :: Prism (Either x a) (Either x b) a b
_Right = Prism Right (first Left)

data Lens s t a b = Lens { view :: s -> a, set :: (s, b) -> t }

_1 :: Lens (a, x) (b, x) a b
_1 = Lens fst (\((_, x), b) -> (b, x))

_2 :: Lens (x, a) (x, b) a b
_2 = Lens snd (first fst)
