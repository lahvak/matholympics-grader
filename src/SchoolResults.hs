module SchoolResults
    ( SchoolResult(..)
    , TopTeams(..)
    , extractSchoolRestults
    , compileTopTeams
) where

import Levels (L1(..), L2(..), Combined(..))
import Schools (Schools, getSchoolName)
import Grader (StudentResult(..))
import Sorting

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import Data.Maybe (isJust, fromJust)
import Data.List (sortOn)
import qualified Data.Ord

data SchoolResult = SchoolResult
                        { schoolName :: Text
                        , schoolCode :: Text
                        , level1 :: Maybe (L1 SortedResults)
                        , level2 :: Maybe (L2 SortedResults)
                        } deriving (Show)

data SchoolScores = SchoolScores
                        { topLevel1 :: Maybe (L1 [Int])
                        , topLevel2 :: Maybe (L2 [Int])
                        , topCombined :: Maybe (Combined [Int])
                        } deriving(Show)

data TopTeams = TopTeams
                    { teamsLevel1 :: L1 [(Text, [Int])]
                    , teamsLevel2 :: L2 [(Text, [Int])]
                    , teamsCombined :: Combined [(Text, [Int])]
                    } deriving(Show)

extractSchoolRestults :: Schools -> (L1 SortedResults, L2 SortedResults) -> Map Text SchoolResult
extractSchoolRestults schools (l1, l2) = Map.unionWith combinelevels l1map l2map
    where
        combinelevels (SchoolResult n1 c1 l11 l21) (SchoolResult _ _ l12 l22) = SchoolResult n1 c1 (l11 <|> l12) (l21 <|> l22)
        l1map = fillrestl1 $ fmap (splitSorted resultSchool) l1
        l2map = fillrestl2 $ fmap (splitSorted resultSchool) l2
        fillrestl1 (L1 rmap) = Map.mapWithKey (\k sr -> SchoolResult (getSchoolName k schools) k (Just (L1 sr)) Nothing) rmap
        fillrestl2 (L2 rmap) = Map.mapWithKey (\k sr -> SchoolResult (getSchoolName k schools) k Nothing (Just (L2 sr))) rmap

topScores :: SortedResults -> [Int]
topScores = uncurry (:) . swap . fmap sum . swap . splitAt 3 . take 5 . mapOverSorted score

l1Scores :: SchoolResult -> Maybe (L1 [Int])
l1Scores sr = fmap topScores <$> level1 sr

l2Scores :: SchoolResult -> Maybe (L2 [Int])
l2Scores sr = fmap topScores <$> level2 sr

combinedScores :: L1 [Int] -> L2 [Int] -> Combined [Int]
combinedScores (L1 l1) (L2 l2) = Combined . take 3 $ zipWith (+) (l1 ++ repeat 0) (l2 ++ repeat 0)

schoolTopScores :: SchoolResult -> SchoolScores
schoolTopScores sr = SchoolScores topl1 topl2 (combinedScores <$> topl1 <*> topl2)
    where
        topl1 = l1Scores sr
        topl2 = l2Scores sr

mapToDescList :: (Ord k, Ord a) => Map k (Maybe a) -> [(k, a)]
mapToDescList = sortOn (Data.Ord.Down . snd) . Map.assocs . Map.map fromJust . Map.filter isJust

mapToDescListWith :: (Ord k, Ord a) => (b -> Maybe a) -> Map k b -> [(k, a)]
mapToDescListWith f = mapToDescList . Map.map f

topTeamsL1 :: Map Text SchoolScores -> L1 [(Text, [Int])]
topTeamsL1 = L1 . map (fmap fromL1) . mapToDescListWith topLevel1

topTeamsL2 :: Map Text SchoolScores -> L2 [(Text, [Int])]
topTeamsL2 = L2 . map (fmap fromL2) . mapToDescListWith topLevel2

topTeamsCombined :: Map Text SchoolScores -> Combined [(Text, [Int])]
topTeamsCombined = Combined . map (fmap fromCombined) . mapToDescListWith topCombined

compileTopTeams :: Map Text SchoolResult -> TopTeams
compileTopTeams mr = TopTeams (topTeamsL1 tscores) (topTeamsL2 tscores) (topTeamsCombined tscores)
    where
        tscores = Map.map schoolTopScores mr

