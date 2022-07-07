{-# LANGUAGE OverloadedStrings #-}
module StudentData
    ( Student(..)
    , parseStudentDataWithInput
    ) where

import StudentData.Internal
import Schools (Schools)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

parseStudentDataWithInput :: Text -> Text -> Schools-> IO Student
parseStudentDataWithInput name code schools =
    case parseStudentData name code schools of
        Right stud -> return stud
        Left msg -> do
            TIO.putStrLn $ Text.concat [msg, " in student code " , code , " for student " , name , "!"]
            putStrLn "Please enter a new student code!"
            putStrLn "You can enter --- for school code.  The test will be graded but not associated with a school."
            putStrLn "You can enter - for level.  The test will be completely ignored."
            putStr "New code: "
            newcode <- TIO.getLine
            parseStudentDataWithInput name newcode schools
