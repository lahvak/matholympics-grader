{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Grader.Internal where

import ScantronParser (Option, Exam(..))
import Levels (Level(..))
import StudentData (Student(..), parseStudentDataWithInput)
import Schools (Schools)

-- Set
import Data.Set (Set)
import qualified Data.Set as Set
-- vector
import Data.Text (Text)

data Answer = ExactMatch (Set Option) | Subset (Set Option) deriving(Show, Eq)

data StudentResult =
    StudentResult
        { resultName :: !Text
        , resultSchool :: !Text
        , resultLevel :: Level
        , resultGrade :: !Text
        , score :: Int
        }
        deriving (Show, Eq)

scoreProblem :: Answer -> Set Option -> Int
scoreProblem (ExactMatch s) = fromEnum . (== s)
scoreProblem (Subset s) = (fromEnum .) . (&&) <$> (`Set.isSubsetOf` s) <*> (not . Set.null)

scoreExam :: [Answer] -> [Set Option] -> Int
scoreExam correct given = sum $ zipWith scoreProblem correct given

gradeExam :: [Answer] -> [Answer] -> Student -> [Set Option] -> Maybe StudentResult
gradeExam level1cor level2cor stud given = gradefun <$> level stud
    where
        gradefun One = StudentResult (studentName stud) (school stud) One (grade stud) (scoreExam level1cor given)
        gradefun Two = StudentResult (studentName stud) (school stud) Two (grade stud) (scoreExam level2cor given)

processExam :: Schools -> [Answer] -> [Answer] ->  Exam -> IO (Maybe StudentResult)
processExam schools corl1 corl2 (Exam ename esid eanswers) = do
    stud <- parseStudentDataWithInput ename esid schools
    return $ gradeExam corl1 corl2 stud eanswers
