{-# LANGUAGE LambdaCase #-}

{-|
Module      : Data.Iff
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Unwrap 'Maybe's with predicates.
-}

module Data.Iff where

import           Control.Monad (ap)
import           Data.Bool     (bool)
import           Data.Maybe    (fromMaybe)

-- | Given a default value @d@, a predicate @p@ and a value @x@, performs
-- case analysis on @iff p x@. When the result is @Nothing@, returns @d@, and
-- when it's @Just x@ returns @x@.
fromIff :: a -> (a -> Bool) -> a -> a
fromIff d = fromMaybe d .: iff
-- fromIff d p x = case iff p x of { Nothing -> d; Just _ -> x}

-- | Given a predicate @p@ and a value @x@, returns @Just x@ iff @p x@ holds,
-- otherwise @Nothing@.
iff :: (a -> Bool) -> a -> Maybe a
iff = ap (bool Nothing . Just)
-- iff p x = bool Nothing (Just x) (p x)
-- iff p x = case p x of { False -> Nothing; True -> Just x}

-- | Given a predicate @p@, a value @x@ and a desired result @d@, calls 'iff',
-- except iff @p x@ holds returns @Just d@, otherwise @Nothing@.
iffThen :: (a -> Bool) -> a -> b -> Maybe b
-- iffThen = ((. Just) . (>>)) .: iff
iffThen p x = (iff p x >>) . Just
-- iffThen p x d = iff p x >> Just d

iffThenElse :: (a -> Bool) -> a -> a -> a -> Maybe a
iffThenElse p x y z = Just $ bool z y (p x)
-- iffThenElse = ((Just .: flip fromMaybe) .) .: iffThen
-- iffThenElse p x y z = bool (Just z) (Just y) . isJust $ iffThen p x y
-- iffThenElse p x y z = Just $ fromMaybe z $ iffThen p x y
-- iffThenElse p x y z = bool (Just z) (Just y) (isJust (iff p x))
-- iffThenElse p x y z = case iff p x of { Nothing -> Just z; Just _ -> Just y}
-- iffThenElse p x y z
--   | p x = Just y
--   | otherwise = Just z

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- |
--
-- > (f .:. g) x y = f x (g y)
--
-- or,
--
-- > f .:. g = (. g) . f
-- (.:.) :: (b -> c -> a) -> (d -> c) -> b -> d -> a
-- (.:.) = flip . ((.) .)
