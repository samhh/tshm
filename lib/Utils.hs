module Utils where

import           Data.List (partition)
import           Prelude

-- | Conditionally apply an endomorphism.
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f = f
applyWhen False _ = id

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

-- Borrowed from: https://hackage.haskell.org/package/intro-0.9.0.0/docs/Intro.html#v:-60--62--94-
-- | Semigroup concat lifted to an applicative context.
(<>^) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = liftA2 (<>)
infixr 6 <>^
{-# INLINE (<>^) #-}
