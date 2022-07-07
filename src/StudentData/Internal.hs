{-# LANGUAGE OverloadedStrings #-}
module StudentData.Internal where

import Levels (Level(..))
import Schools (Schools, isValidSchool)

import Data.Text (Text)
import qualified Data.Text as Text

data Student =
    Student
        { studentName :: !Text
        , school :: !Text
        , level :: Maybe Level
        , grade :: !Text
        }
        deriving (Eq, Show)


parseStudentData :: Text -> Text -> Schools -> Either Text Student
parseStudentData name code schools =
    Student <$> Right sname <*> sschool <*> slevel <*> Right sgrade
    where
        sname = fixPersonalName name
        schoolcode = Text.take 3 code
        levelno = Text.take 1 . Text.drop 3 $ code
        sgrade = Text.take 2 . Text.drop 4 $ code
        sschool = if isValidSchool schoolcode schools then Right schoolcode else Left "Invalid school code"
        slevel = case levelno of
            "1" -> Right $ Just One
            "2" -> Right $ Just Two
            "-" -> Right Nothing
            _ -> Left "Invalid level"

fixPersonalName :: Text -> Text
fixPersonalName name = if Text.null lastname || Text.null rests then casedName else lastname <> "," <> rests
    where
        (lastname, rests) = Text.breakOn " " casedName
        casedName = Text.toTitle name

