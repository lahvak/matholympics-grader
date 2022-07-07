module Main where

import Levels (Level(..))
import Schools (readSchools, schoolsToStrs)
import MCAFiles (readMCAFile)
import ScantronParser (decodeFile)
import Grader (processExams)
import Sorting (splitAndSort)
import SchoolResults (extractSchoolRestults, compileTopTeams, TopTeams(..))
import Reporting
import AppOptions (getOptions, DataFile(..), getDataFile)

import System.Exit

readWithError :: (FilePath -> IO(Either String t)) -> FilePath -> IO t
readWithError fun fp = do
    putStrLn $ "Reading file " <> fp
    readresult <- fun fp
    case readresult of
        Right contents -> return contents
        Left msg -> do
            putStrLn msg
            exitFailure

main :: IO ()
main = do
    options <- getOptions

    putStrLn "Reading school codes"
    schools <- readSchools $ getDataFile SchoolsFile options
    if length schools > 1 then
        mapM_ putStrLn $ schoolsToStrs schools
    else
        putStrLn "School list is empty!" >> exitFailure

    level1_correct <- readWithError readMCAFile $ getDataFile (MCA One) options

    level2_correct <- readWithError readMCAFile $ getDataFile (MCA Two) options

    exams <- readWithError decodeFile $ getDataFile DataFile options

    putStrLn "Done reading"

    scores <- processExams schools level1_correct level2_correct exams

    putStrLn "Done scoring"

    let levels = splitAndSort scores

    putStrLn "Done sorting"

    let schoolResults = extractSchoolRestults schools levels

    let teams = compileTopTeams schoolResults

    renderL1Individual options schools (fst levels)
    renderL2Individual options schools (snd levels)
    renderL1Team options schools (teamsLevel1 teams)
    renderL2Team options schools (teamsLevel2 teams)
    renderCombinedTeam options schools (teamsCombined teams)
    renderSchoolResults options schoolResults

