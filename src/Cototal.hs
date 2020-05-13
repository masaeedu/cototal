module Cototal where

import Data.Function ((&))
import Control.Arrow ((&&&))

import Lens

class Full a
  where
  trivial :: x -> a

instance Full ()
  where
  trivial = const ()

instance (Full a, Full b) => Full (a, b)
  where
  trivial = trivial &&& trivial

_cocase :: Full a => x -> a
_cocase = trivial

at :: Lens s t () b -> (i -> b) -> (i -> s) -> i -> t
at (Lens _ s) l r = s . (r &&& l)

data Thing = Thing Char Int

cototal :: Thing -> (Char, Int)
cototal = _cocase
  & at _1 (\(Thing c _) -> c)
  & at _2 (\(Thing _ i) -> i)
