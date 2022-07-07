{-# LANGUAGE OverloadedStrings #-}
module Reporting
    ( renderL1Individual
    , renderL2Individual
    , renderL1Team
    , renderL2Team
    , renderCombinedTeam
    , renderSchoolResults
    ) where

import Sorting (SortedResults, mapOverSorted)
import Grader (StudentResult(..))
import Schools (Schools, getSchoolName)
import Levels (L1(..), L2(..), Combined(..))
import SchoolResults (SchoolResult(..))
import AppOptions (Options, TeXPrefix(..), getTeXFile)

import Text.LaTeX
import Text.LaTeX.Packages.LongTable
import Text.LaTeX.Packages.Fancyhdr
import Text.LaTeX.Packages.Geometry

import Data.Map (Map)
import qualified Data.Map as Map

renderTeXFile :: Options -> TeXPrefix -> String -> LaTeX -> IO ()
renderTeXFile os tp level = renderFile (getTeXFile tp os level)

thePreamble :: LaTeX
thePreamble = documentclass [] report <>
    usepackage [] longtablep <>
    usepackage [] fancyhdr <>
    usepackage ["margin=1in"] geometry <>
    pagestyle fancy

sortedResultsTableCommon :: [TableSpec] -> LaTeX -> (StudentResult -> LaTeX) -> SortedResults -> LaTeX
sortedResultsTableCommon columns header renderfun sr =
    longtable Nothing (Separator (raw "\\extracolsep{\\fill}"):columns) $
        header <> raw "\\endfirsthead " <>
        header <> raw "\\endhead " <>
        hline <>
        multicolumn (length columns) [RightColumn] (small (textsl "Continued on the next page")) <> lnbk <> raw "\\endfoot " <>
        hline <> endlastfoot <>
        mconcat (mapOverSorted renderfun sr)


renderStudentResult :: Schools -> StudentResult -> LaTeX
renderStudentResult schools (StudentResult n s _ g c) =
    texy n &
    texy (getSchoolName s schools) &
    texy g &
    texy c <>
    lnbk

renderStudentResultNoSchool :: StudentResult -> LaTeX
renderStudentResultNoSchool (StudentResult n _ _ g c) =
    texy n &
    texy g &
    texy c <>
    lnbk

mkTableHead :: [LaTeX] -> LaTeX
mkTableHead [] = mempty
mkTableHead l =
    foldl1 (&) (map textbf l) <>
    lnbk <>
    hline

individualTableHead :: LaTeX
individualTableHead = mkTableHead ["Name", "School", "Grade", "Score"]

sortedResultsTable :: Schools -> SortedResults -> LaTeX
sortedResultsTable schools = sortedResultsTableCommon
    [LeftColumn, LeftColumn, RightColumn, RightColumn]
    individualTableHead
    (renderStudentResult schools)

theIndividualHead :: Int -> LaTeX
theIndividualHead n =
    center (large2 "SVSU Math Olympics" <> lnbk <> "Individual results -- Level " <> texy n <> lnbk <> today)

theL1IndividualBody :: Schools -> L1 SortedResults -> LaTeX
theL1IndividualBody schools (L1 sr) = theIndividualHead 1 <> sortedResultsTable schools sr

l1Individual :: Schools -> L1 SortedResults -> LaTeX
l1Individual schools l1sr = thePreamble <> document (theL1IndividualBody schools l1sr)

renderL1Individual :: Options -> Schools -> L1 SortedResults -> IO ()
renderL1Individual opts schools = renderTeXFile opts Individual "L1" . l1Individual schools

theL2IndividualBody :: Schools -> L2 SortedResults -> LaTeX
theL2IndividualBody schools (L2 sr) = theIndividualHead 2 <> sortedResultsTable schools sr

l2Individual :: Schools -> L2 SortedResults -> LaTeX
l2Individual schools l2sr = thePreamble <> document (theL2IndividualBody schools l2sr)

renderL2Individual :: Options -> Schools -> L2 SortedResults -> IO ()
renderL2Individual opts schools = renderTeXFile opts Individual "L2" . l2Individual schools

renderTeam :: Schools -> Text -> [Int] -> LaTeX
renderTeam schools code l =
    texy (getSchoolName code schools) &
    foldl1 (&) (take 3 (map texy l ++ repeat mempty)) <>
    lnbk

teamTableHead :: LaTeX
teamTableHead = mkTableHead ["School", "top 3", "fourth", "fifth"]

teamTable :: Schools -> [(Text, [Int])] -> LaTeX
teamTable _ [] = mempty
teamTable schools l =
    longtable Nothing [Separator (raw "\\extracolsep{\\fill}"), LeftColumn, RightColumn, RightColumn, RightColumn] $
        teamTableHead <> raw "\\endfirsthead" <>
        teamTableHead <> raw "\\endhead" <>
        hline <>
        multicolumn 4 [RightColumn] (small (textsl "Continued on the next page")) <> lnbk <> raw "\\endfoot" <>
        hline <> endlastfoot <>
        mconcat (map (uncurry (renderTeam schools)) l)

theTeamHead :: LaTeX -> LaTeX
theTeamHead t =
    center (large2 "SVSU Math Olympics" <> lnbk <> "Team results -- " <> t <> lnbk <> today)

theL1TeamBody :: Schools -> L1 [(Text, [Int])] -> LaTeX
theL1TeamBody schools (L1 teams) = theTeamHead "Level 1" <> teamTable schools teams

l1Team :: Schools -> L1 [(Text, [Int])] -> LaTeX
l1Team schools l1sr = thePreamble <> document (theL1TeamBody schools l1sr)

renderL1Team :: Options -> Schools -> L1 [(Text, [Int])] -> IO ()
renderL1Team opts schools = renderTeXFile opts Team "L1" . l1Team schools

theL2TeamBody :: Schools -> L2 [(Text, [Int])] -> LaTeX
theL2TeamBody schools (L2 teams) = theTeamHead "Level 2" <> teamTable schools teams

l2Team :: Schools -> L2 [(Text, [Int])] -> LaTeX
l2Team schools l2sr = thePreamble <> document (theL2TeamBody schools l2sr)

renderL2Team :: Options -> Schools -> L2 [(Text, [Int])] -> IO ()
renderL2Team opts schools = renderTeXFile opts Team "L2" . l2Team schools

theCombinedTeamBody :: Schools -> Combined [(Text, [Int])] -> LaTeX
theCombinedTeamBody schools (Combined teams) = theTeamHead "Combined Levels" <> teamTable schools teams

combinedTeam :: Schools -> Combined [(Text, [Int])] -> LaTeX
combinedTeam schools combl = thePreamble <> document (theCombinedTeamBody schools combl)

renderCombinedTeam :: Options -> Schools -> Combined [(Text, [Int])] -> IO ()
renderCombinedTeam opts schools = renderTeXFile opts Team "Combined" . combinedTeam schools

individualTableNoSchoolHead :: LaTeX
individualTableNoSchoolHead = mkTableHead ["Name", "Grade", "Score"]

sortedResultsTableNoSchool :: SortedResults -> LaTeX
sortedResultsTableNoSchool = sortedResultsTableCommon
        [LeftColumn, RightColumn, RightColumn]
        individualTableNoSchoolHead
        renderStudentResultNoSchool

schoolSortedResultsL1 :: L1 SortedResults -> LaTeX
schoolSortedResultsL1 (L1 sr) =
    subsection' "Level 1" <> sortedResultsTableNoSchool sr

schoolSortedResultsL2 :: L2 SortedResults -> LaTeX
schoolSortedResultsL2 (L2 sr) =
    subsection' "Level 2" <> sortedResultsTableNoSchool sr

schoolResultList :: SchoolResult -> LaTeX
schoolResultList (SchoolResult n _ l1 l2) =
    newpage <> section' (texy n) <>
    maybe mempty schoolSortedResultsL1 l1 <>
    maybe mempty schoolSortedResultsL2 l2

schoolsResBody :: Map Text SchoolResult -> LaTeX
schoolsResBody = mconcat . map schoolResultList . Map.elems

schoolRes :: Map Text SchoolResult -> LaTeX
schoolRes m = thePreamble <> document (schoolsResBody m)

renderSchoolResults :: Options -> Map Text SchoolResult -> IO ()
renderSchoolResults opts = renderFile (getTeXFile Schools opts "") . schoolRes
