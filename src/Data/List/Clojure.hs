{-|
Module      : Data.List.Clojure
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Clojure-inspired list functions for Haskell.
-}

module Data.List.Clojure ( butLast
                         , dropLast1
                         , dropLast
                         , genericDropLast
                         , takeNth
                         , genericTakeNth ) where

import           Control.Monad             (ap)
import qualified Data.List.Clojure.Generic as G

-- | The 'butLast' function is an alias of 'init' and yields
-- the same result as 'dropLast1'.
butLast :: [a] -> [a]
butLast = init

-- | Return all but the last element of a given list.
dropLast1 :: [a] -> [a]
dropLast1 = dropLast 1

-- | Given a number @n@ and a list @l@, return a list of
-- all but the last @n@ elements in @l@.
dropLast :: Int -> [a] -> [a]
dropLast = ap (zipWith const) . drop

-- | The 'genericDropLast' function is an overloaded version of 'dropLast',
-- which accepts any 'Integral' value as the number of elements to drop.
genericDropLast :: Integral a => a -> [b] -> [b]
genericDropLast = G.dropLast

-- | Return a list of every nth element of a given list.
takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth n xs = head xs : takeNth n (drop n xs)

-- | The 'genericTakeNth' function is an overloaded version of 'takeNth', which
-- accepts any 'Integral' value as the nth elements to take.
genericTakeNth :: Integral b => b -> [a] -> [a]
genericTakeNth = G.takeNth
