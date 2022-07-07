{-# LANGUAGE OverloadedStrings #-}
module Schools
    ( Schools
    , isValidSchool
    , parseSchoolList
    , readSchools
    , schoolsToStrs
    , getSchoolName
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

type Schools = Map Text Text

showSchool :: Text -> Text -> Text
showSchool code school = code <> ": " <> school

schoolsToStrs :: Schools -> [String]
schoolsToStrs = map (Text.unpack . uncurry showSchool) . Map.toAscList . Map.delete "---"

isValidSchool :: Text -> Schools -> Bool
isValidSchool = Map.member

getSchoolName :: Text -> Schools -> Text
getSchoolName = Map.findWithDefault "Invalid school"

parseSchool :: Text -> Maybe (Text, Text)
parseSchool line =
    case Text.splitOn "," line of
        [school, code] -> Just (Text.strip code, Text.strip school)
        _ -> Nothing

parseSchoolList :: [Text] -> Schools
parseSchoolList = Map.insert "---" "Invalid school" . Map.fromList . mapMaybe parseSchool

readSchools :: FilePath -> IO Schools
readSchools = fmap (parseSchoolList . Text.lines) . TIO.readFile
