module Utils where

import           Data.List (partition)
import           Prelude

-- | Like `groupBy`, but groups non-sibling items towards the left-most match,
-- and doesn't nest the result.
arrangeBy :: (a -> a -> Bool) -> [a] -> [a]
arrangeBy _ []     = []
arrangeBy f (x:xs) =
  let (ys, zs) = partition (f x) xs
   in x : ys <> arrangeBy f zs

-- | Concatenates sibling items when the predicate matches.
concatWhereBy :: (a -> a -> a) -> (a -> a -> Bool) -> [a] -> [a]
concatWhereBy _ _ []  = []
concatWhereBy _ _ [x] = [x]
concatWhereBy f p (x:y:zs)
  | p x y     =     concatWhereBy f p (f x y : zs)
  | otherwise = x : concatWhereBy f p (y     : zs)

