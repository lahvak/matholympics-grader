module MCAFiles.Internal where

import ScantronParser (Option(..))
import Grader (Answer(..))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char (toUpper)

import Control.Monad (zipWithM)

parseOptions :: String -> Either String (Set Option)
parseOptions = fmap Set.fromList . mapM (pop . toUpper)
    where
        pop 'A' = Right A
        pop 'B' = Right B
        pop 'C' = Right C
        pop 'D' = Right D
        pop 'E' = Right E
        pop x = Left $ "Invalid character: " ++ [x]

parseMCALine :: String -> Either String Answer
-- Single character or !multiple characters = exact match
parseMCALine ('!':opts) = ExactMatch <$> parseOptions opts
parseMCALine [x] = ExactMatch <$> parseOptions [x]
-- Empty list yields Left rather than Right empty set
parseMCALine [] = Left "Empty list of options"
parseMCALine x = Subset <$> parseOptions x

parseMCAList :: [String] -> Either String [Answer]
parseMCAList [] = Left "Empty list"
parseMCAList l = zipWithM parseline [1..] l
    where
        parseline :: Int -> String -> Either String Answer
        parseline n s = case parseMCALine s of
                        Right a -> Right a
                        Left e -> Left $ e ++ " on line " ++ show n


