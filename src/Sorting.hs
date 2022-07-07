module Sorting (
    SortedResults
    , splitAndSort
    , mapOverSorted
    , splitSorted
) where

import Grader (StudentResult(..))
import Levels (Level(..), L1(..), L2(..))

-- Map
import Data.Map (Map)
import qualified Data.Map as Map
-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Bifunctor (bimap)

newtype SortedResults = SortedResults [StudentResult] deriving(Show)

mapOverSorted :: (StudentResult -> a) -> SortedResults -> [a]
mapOverSorted f (SortedResults res) = map f res

splitSorted :: (Ord k) => (StudentResult -> k) -> SortedResults -> Map k SortedResults
splitSorted f (SortedResults res) = fmap SortedResults . foldr (\r -> Map.alter (altfun r) (f r)) Map.empty $ res
    where
        altfun r Nothing = Just [r]
        altfun r (Just l) = Just $ r:l

splitLevels :: Vector StudentResult -> (L1 (Vector StudentResult), L2 (Vector StudentResult))
splitLevels = bimap L1 L2 . Vector.partition ((== One) . resultLevel)

sortByScore :: Vector StudentResult -> SortedResults
sortByScore = SortedResults . concat . Map.elems . Vector.foldr (\r -> Map.alter (altfun r) (25 - score r)) Map.empty
    where
        altfun r Nothing = Just [r]
        altfun r (Just l) = Just $ r:l

splitAndSort :: Vector StudentResult -> (L1 SortedResults, L2 SortedResults)
splitAndSort = bimap (fmap sortByScore) (fmap sortByScore) . splitLevels
