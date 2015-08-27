{-|
Module      : Data.List.Clojure
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Generic variants of Clojure-inspired list functions for Haskell.
-}

module Data.List.Clojure.Generic where

import           Control.Monad (ap)
import qualified Data.List     as L

-- | Return all but the last element of a given list.
dropLast1 :: [a] -> [a]
dropLast1 = dropLast (1 :: Int)

-- | Given a number @n@ and a list @l@, return a list of
-- all but the last @n@ elements in @l@.
dropLast :: Integral b => b -> [a] -> [a]
dropLast = ap (zipWith const) . L.genericDrop

-- | Return a list of every nth element of a given list.
takeNth :: Integral b => b -> [a] -> [a]
takeNth _ [] = []
takeNth n xs = head xs : takeNth n (L.genericDrop n xs)
