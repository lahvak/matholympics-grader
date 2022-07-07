{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Levels
import StudentData.Internal
import ScantronParser.Internal
import Grader.Internal
import Schools
import MCAFiles.Internal
import Test.HUnit

import Data.Either (isLeft)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Map as Map

-- StudentData:

studentDataTests :: Test
studentDataTests = TestLabel "Tests of parsing student data" ( TestList [
    parseStudentValid
    , parseStudentWrongLevel
    , parseStudentWrongSchool
    , parseStudentEmpty
    , parseStudentNoGrade
    ])

parseStudentValid :: Test
parseStudentValid = TestCase $ assertEqual
    "Parsing valid student data"
    (Right $ Student "Ipse, Egon" "001" (Just One) "12")
    (parseStudentData "IPSE EGON" "0011129" $ Map.fromList [("001", "Sladarna")])

parseStudentWrongLevel :: Test
parseStudentWrongLevel = TestCase $ assertEqual
    "Parsing student data with wrong level"
    (Left "Invalid level")
    (parseStudentData "IPSE EGON" "0010129" $ Map.fromList [("001", "Sladarna")])

parseStudentWrongSchool :: Test
parseStudentWrongSchool = TestCase $ assertEqual
    "Parsing student data with wrong school code"
    (Left "Invalid school code")
    (parseStudentData "IPSE EGON" "0020129" $ Map.fromList [("001", "Sladarna")])

parseStudentEmpty :: Test
parseStudentEmpty = TestCase $ assertBool
    "Parsing student data with empty data"
    (isLeft $ parseStudentData "IPSE EGON" "" $ Map.fromList [("001", "Sladarna")])

parseStudentNoGrade :: Test
parseStudentNoGrade = TestCase $ assertEqual
    "Parsing student data with no grade data"
    (Right $ Student "Ipse, Egon" "001" (Just Two) "")
    (parseStudentData "IPSE EGON" "0012" $ Map.fromList [("001", "Sladarna")])

-- ScantronParser

parsingTests :: Test
parsingTests = TestLabel "Tests of scantron parser" ( TestList [
        parseSimple
      , parseBlank
      , parseMulti
      , parseInvalid
      , parseShortID
      , parseLongID
      , parseCSVError
   ])

parseSimple :: Test
parseSimple = TestCase $ assertEqual
    "A simple straightforward case"
    (Right $
        Vector.fromList [ Exam 
            { name = "MARTY BLOOM" 
            , sid = "2345678" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ B ]
                , Set.fromList [ C ]
                , Set.fromList [ D ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ A ]
                , Set.fromList [ B ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                ] 
            } 
        ])
    (decodeRec "MARTY BLOOM       ,          2345678,A, A ,B,C,D,C,E,C,E,C,A,B,D,D,D,D,D,D,D,D,D,D,D,D,D,,,,,,")

parseBlank :: Test
parseBlank = TestCase $ assertEqual
    "Parse a record with blank entries"
    (Right $
        Vector.fromList [ Exam 
            { name = "MARTY BLANK" 
            , sid = "2345678" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList []
                , Set.fromList [ C ]
                , Set.fromList []
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList []
                , Set.fromList [ A ]
                , Set.fromList [ B ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                ] 
            } 
        ])
    (decodeRec "MARTY BLANK       ,          2345678,A,A,,C,BLANK,C,E,C,E,,A,B,D,D,D,D,D,D,D,D,D,D,D,D,D")

parseMulti :: Test
parseMulti = TestCase $ assertEqual 
    "Parse record with multiple letters selected for an item"
    (Right $ Vector.fromList
        [ Exam 
            { name = "MULTI BLOOM" 
            , sid = "2345678" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList 
                    [ A
                    , B
                    , C
                    ] 
                , Set.fromList [ C ]
                , Set.fromList [ B ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList 
                    [ B
                    , D
                    , E
                    ] 
                , Set.fromList [ A ]
                , Set.fromList [ B ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                ] 
            } 
        ])
    (decodeRec "MULTI BLOOM       ,          2345678,A,A,\"(A,B,C)\",C,B,C,E,C,E,\"(D, E, B)\",A,B,D,D,D,D,D,D,D,D,D,D,D,D,D")

parseInvalid :: Test
parseInvalid = TestCase $ assertEqual 
    "Parse record with invalid items"
    (Right $ Vector.fromList
        [ Exam 
            { name = "MARTY BAD" 
            , sid = "2345678" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ Invalid ]
                , Set.fromList [ C ]
                , Set.fromList [ D ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ E ]
                , Set.fromList [ C ]
                , Set.fromList [ A ]
                , Set.fromList [ B ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                , Set.fromList [ D ]
                ] 
            } 
        ])
    (decodeRec "MARTY BAD         ,          2345678,A,A,X,C,D,C,E,C,E,C,A,B,D,D,D,D,D,D,D,D,D,D,D,D,D")

parseShortID :: Test
parseShortID = TestCase $ assertEqual 
    "Parse record with short ID"
    (Right $ Vector.fromList
        [ Exam 
            { name = "SHORTY BLOOM" 
            , sid = "  23456" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                ] 
            } 
        ])
    (decodeRec "SHORTY BLOOM      ,          23456,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A")

parseLongID :: Test
parseLongID = TestCase $ assertEqual 
    "Parse record with long ID"
    (Right $ Vector.fromList
        [ Exam 
            { name = "MARTY LONG" 
            , sid = "1234567" 
            , answers = 
                [ Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                , Set.fromList [ A ]
                ] 
            } 
        ])
    (decodeRec "MARTY LONG        ,        345677041234567,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A")

parseCSVError :: Test
parseCSVError = TestCase $ assertBool
    "Parsing mapformed record should fail"
    (isLeft $ decodeRec "MIGHTY WRONG")

-- Grader

gradingTests :: Test
gradingTests = TestLabel "Tests of grading" ( TestList [
        testExactMatchTrue
      , testExactMatchFalse
      , testSubsetTrue
      , testSubsetFalse
      , testSubsetEmpty
      , testSubsetEqual
      , testSum
      , testGraderL1
      , testGraderL2
      , testGraderWrongLevel
   ])
    

testExactMatchTrue :: Test
testExactMatchTrue = TestCase $ assertEqual
    "Exact match that is correct"
    1
    (scoreProblem (ExactMatch $ Set.fromList [A,B,C]) (Set.fromList [A,B,C]))

testExactMatchFalse :: Test
testExactMatchFalse = TestCase $ assertEqual
    "Exact match that is incorrect"
    0
    (scoreProblem (ExactMatch $ Set.fromList [A,B,C]) (Set.fromList [A,B]))

testSubsetTrue :: Test
testSubsetTrue = TestCase $ assertEqual
    "Subset that is correct"
    1
    (scoreProblem (Subset $ Set.fromList [A,B,C]) (Set.fromList [A,B]))

testSubsetFalse :: Test
testSubsetFalse = TestCase $ assertEqual
    "Subset that is incorrect"
    0
    (scoreProblem (Subset $ Set.fromList [A,B]) (Set.fromList [A,C]))

testSubsetEmpty :: Test
testSubsetEmpty = TestCase $ assertEqual
    "Subset that is empty"
    0
    (scoreProblem (Subset $ Set.fromList [A,B]) (Set.fromList []))

testSubsetEqual :: Test
testSubsetEqual = TestCase $ assertEqual
    "Subset that is equal"
    1
    (scoreProblem (Subset $ Set.fromList [A,B,C]) (Set.fromList [A,B,C]))

testSum :: Test
testSum = TestCase $ assertEqual
    "Score a whole exam"
    2
    (scoreExam
        [ ExactMatch $ Set.fromList [A,B,C]
        , ExactMatch $ Set.fromList [A,D]
        , Subset $ Set.fromList [A,C,D]
        , Subset $ Set.fromList [A,B]
        ]
        [ Set.fromList [A,B,C]
        , Set.fromList [A]
        , Set.fromList [A,C]
        , Set.empty
        ])

testGraderL1 :: Test
testGraderL1 = TestCase $ assertEqual
    "Grade a Level 1 exam"
    (Just $ StudentResult 
            { resultName = "Level1 Stud"
            , resultSchool = "Level1 School"
            , resultGrade = "12"
            , resultLevel = One
            , score = 3
            })
    (gradeExam
        [ ExactMatch $ Set.fromList [A,B,C]
        , ExactMatch $ Set.fromList [A,D]
        , Subset $ Set.fromList [A,C,D]
        , Subset $ Set.fromList [A,B]
        ]
        [ ExactMatch $ Set.fromList [D]
        , ExactMatch $ Set.fromList [B,C]
        , Subset $ Set.fromList [B,E]
        , Subset $ Set.fromList [A,C]
        ]
        (Student
            { studentName = "Level1 Stud"
            , school = "Level1 School"
            , level = Just One
            , grade = "12"
            }
        )
        [ Set.fromList [A,B,C]
        , Set.fromList [A,D]
        , Set.fromList [C]
        , Set.empty
        ])

testGraderL2 :: Test
testGraderL2 = TestCase $ assertEqual
    "Grade a Level 2 exam"
    (Just $ StudentResult 
            { resultName = "Level2 Stud"
            , resultSchool = "Level2 School"
            , resultGrade = "12"
            , resultLevel = Two
            , score = 3
            })
    (gradeExam
        [ ExactMatch $ Set.fromList [D]
        , ExactMatch $ Set.fromList [B,C]
        , Subset $ Set.fromList [B,E]
        , Subset $ Set.fromList [A,C]
        ]
        [ ExactMatch $ Set.fromList [A,B,C]
        , ExactMatch $ Set.fromList [A,D]
        , Subset $ Set.fromList [A,C,D]
        , Subset $ Set.fromList [A,B]
        ]
        (Student
            { studentName = "Level2 Stud"
            , school = "Level2 School"
            , level = Just Two
            , grade = "12"
            }
        )
        [ Set.fromList [A,B,C]
        , Set.fromList [A,D]
        , Set.fromList [C]
        , Set.empty
        ])

testGraderWrongLevel :: Test
testGraderWrongLevel = TestCase $ assertEqual
    "Grade an exam with a wrong level"
    Nothing
    (gradeExam
        [ ExactMatch $ Set.fromList [D]
        , ExactMatch $ Set.fromList [B,C]
        , Subset $ Set.fromList [B,E]
        , Subset $ Set.fromList [A,C]
        ]
        [ ExactMatch $ Set.fromList [A,B,C]
        , ExactMatch $ Set.fromList [A,D]
        , Subset $ Set.fromList [A,C,D]
        , Subset $ Set.fromList [A,B]
        ]
        (Student
            { studentName = "Level2 Stud"
            , school = "Level2 School"
            , level = Nothing
            , grade = "12"
            }
        )
        [ Set.fromList [A,B,C]
        , Set.fromList [A,D]
        , Set.fromList [C]
        , Set.empty
        ])

-- Schools

schoolTests :: Test
schoolTests = TestLabel "Tests of school list" ( TestList [
        testIsValidSchoolValid
      , testIsUnknownSchoolValid
      , testIsInvalidSchoolInvalid
      , testListOneSchool
      , testListMultipleSchools
   ])

testSchoolList :: Schools
testSchoolList = parseSchoolList ["Sky High,001"]

testIsValidSchoolValid :: Test
testIsValidSchoolValid = TestCase $ assertBool
    "Does it recognize a valid school"
    (isValidSchool "001" testSchoolList)

testIsInvalidSchoolInvalid :: Test
testIsInvalidSchoolInvalid = TestCase $ assertBool
    "Does it recognize an invalid school"
    (not $ isValidSchool "002" testSchoolList)

testIsUnknownSchoolValid :: Test
testIsUnknownSchoolValid = TestCase $ assertBool
    "Does it recognize special case of unknown school"
    (isValidSchool "---" testSchoolList)

testListOneSchool :: Test
testListOneSchool = TestCase $ assertEqual
    "Straightforward single school"
    (Map.fromList [("---", "Invalid school"), ("001", "Sladkovskeho Nam")])
    (parseSchoolList ["Sladkovskeho Nam,001"])

testListMultipleSchools :: Test
testListMultipleSchools = TestCase $ assertEqual
    "Multiple schools with various issues"
    (Map.fromList [
        ( "---", "Invalid school")
        , ("001", "Sladkovskeho Nam")
        , ("002", "Bat Masterson Academy")
        , ("004", "Pomocna Skola v Bixley")
    ])
    (parseSchoolList 
        [ "Sladkovskeho Nam,001"
        , "   Bat Masterson Academy      ,   002  "
        , "SASA 003"
        , "Pomocna Skola v Bixley, 004"
        , ""
        , "Smellow, Pugh and Stink, Attorneys at Law"
        ])

-- MCAfiles

mcaTests :: Test
mcaTests = TestLabel "Test of MCA records handling" (TestList [
    testMCAParseSingleValidA
    , testMCAParseSingleValidB
    , testMCAParseSingleValidC
    , testMCAParseSingleValidD
    , testMCAParseSingleValidE
    , testMCAParseSingleInvalid
    , testMCAParseEmpty
    , testMCAParseMultipleValid
    , testMCAParseMultipleInvalid
    , testMCAParseExclValid
    , testMCAParseExclInvalid
    , testMCAListEmpty
    , testMCAListGood
    , testMCAListEmptyItem
    , testMCAListInvalidItem
    ])

testMCAParseSingleValidA :: Test
testMCAParseSingleValidA = TestCase $ assertEqual
    "MCA entry single valid letter"
    (Right $ ExactMatch (Set.fromList [A]))
    (parseMCALine "A")

testMCAParseSingleValidB :: Test
testMCAParseSingleValidB = TestCase $ assertEqual
    "MCA entry single valid letter"
    (Right $ ExactMatch (Set.fromList [B]))
    (parseMCALine "B")

testMCAParseSingleValidC :: Test
testMCAParseSingleValidC = TestCase $ assertEqual
    "MCA entry single valid letter"
    (Right $ ExactMatch (Set.fromList [C]))
    (parseMCALine "C")

testMCAParseSingleValidD :: Test
testMCAParseSingleValidD = TestCase $ assertEqual
    "MCA entry single valid letter"
    (Right $ ExactMatch (Set.fromList [D]))
    (parseMCALine "D")

testMCAParseSingleValidE :: Test
testMCAParseSingleValidE = TestCase $ assertEqual
    "MCA entry single valid letter"
    (Right $ ExactMatch (Set.fromList [E]))
    (parseMCALine "E")

testMCAParseSingleInvalid :: Test
testMCAParseSingleInvalid = TestCase $ assertEqual
    "MCA entry single invalid letter"
    (Left "Invalid character: R")
    (parseMCALine "R")

testMCAParseEmpty :: Test
testMCAParseEmpty = TestCase $ assertEqual
    "Empty MCA entry"
    (Left "Empty list of options")
    (parseMCALine "")

testMCAParseMultipleValid :: Test
testMCAParseMultipleValid = TestCase $ assertEqual
    "MCA multiple valid letters"
    (Right $ Subset (Set.fromList [B, D]))
    (parseMCALine "BD")

testMCAParseMultipleInvalid :: Test
testMCAParseMultipleInvalid = TestCase $ assertEqual
    "MCA multiple invalid letters"
    (Left "Invalid character: R")
    (parseMCALine "ACR")

testMCAParseExclValid :: Test
testMCAParseExclValid = TestCase $ assertEqual
    "MCA exact match valid letters"
    (Right $ ExactMatch (Set.fromList [A, D, E]))
    (parseMCALine "!ADE")

testMCAParseExclInvalid :: Test
testMCAParseExclInvalid = TestCase $ assertEqual
    "MCA exact match invalid letters"
    (Left "Invalid character: R")
    (parseMCALine "!ACR")


testMCAListEmpty :: Test
testMCAListEmpty = TestCase $ assertEqual
    "Empty MCA List"
    (Left "Empty list")
    (parseMCAList [])

testMCAListGood :: Test
testMCAListGood = TestCase $ assertEqual
    "Valid MCA List"
    (Right [
        ExactMatch (Set.fromList [A])
        , ExactMatch (Set.fromList [A, B, C])
        , Subset (Set.fromList [A, B])
        ])
    (parseMCAList ["A", "!ABC", "AB"])

testMCAListEmptyItem :: Test
testMCAListEmptyItem = TestCase $ assertEqual
    "Invalid MCA List (empty item)"
    (Left "Empty list of options on line 3")
    (parseMCAList ["A", "!ABC", "", "AB"])

testMCAListInvalidItem :: Test
testMCAListInvalidItem = TestCase $ assertEqual
    "Invalid MCA List (invalid item)"
    (Left "Invalid character: R on line 3")
    (parseMCAList ["A", "!ABC", "AR", "AB"])

main :: IO Counts
main = runTestTT $ TestList [studentDataTests, parsingTests, gradingTests, schoolTests, mcaTests]
