module Total where

import Data.Void
import Data.Function ((&))

import Lens

class Empty a
  where
  impossible :: a -> x

instance Empty Void
  where
  impossible = absurd

instance (Empty a, Empty b) => Empty (Either a b)
  where
  impossible = either impossible impossible

_case :: Empty a => a -> x
_case = impossible

on :: Prism i r l Void -> (l -> o) -> (r -> o) -> i -> o
on (Prism _ m) l r = either r l . m

total :: Either Char Int -> String     -- Same as:
total = _case                          -- total = \case
  & on _Left  (\c -> replicate 3  c )  --   Left  c -> replicate 3 c
  & on _Right (\n -> replicate n '!')  --   Right n -> replicate n '!'
