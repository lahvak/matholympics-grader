module AppOptions
    ( Options
    , getOptions
    , DataFile(..)
    , TeXPrefix(..)
    , getDataFile
    , getTeXFile
    ) where

import Levels (Level(..))

import Options.Applicative
import Data.Semigroup ((<>))
import System.FilePath ((</>), (<.>))
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing)

data Options = Options
    { dataFile :: FilePath
    , schoolCodes :: FilePath
    , l1MCA :: FilePath
    , l2MCA :: FilePath
    , dataDir :: FilePath
    , texDir :: FilePath
    }

options :: Parser Options
options = Options
    <$> strOption 
            ( long "data"
            <> short 'd'
            <> metavar "CSVFILE"
            <> help "CSV data file from SCANTRON machine"
            <> showDefault
            <> value "data.csv"
            )
    <*> strOption
            ( long "schools"
            <> long "codes"
            <> short 'c'
            <> metavar "CODEFILE"
            <> help "File containing schools and school codes"
            <> showDefault
            <> value "codes.txt"
            )
    <*> strOption
            ( long "mca1"
            <> metavar "MCAFILE"
            <> help "MCA (correct answers) file for level 1"
            <> showDefault
            <> value "level1.mca"
            )
    <*> strOption
            ( long "mca2"
            <> metavar "MCAFILE"
            <> help "MCA (correct answers) file for level 2"
            <> showDefault <> value "level2.mca"
            )
    <*> strOption
            ( long "datadir"
            <> metavar "DATADIR"
            <> help "Directory containing data files. Defaults to the current working directory."
            <> value ""
            )
    <*> strOption
            ( long "TeXdir"
            <> metavar "TEXDIR"
            <> help "Directory where the generated TeX files should be placed. Defaults to the current working directory."
            <> value ""
            )

data DataFile = DataFile | SchoolsFile | MCA Level

data TeXPrefix = Individual | Team | Schools deriving(Show)

opts :: ParserInfo Options
opts = info (options <**> helper)
    ( fullDesc
    <> progDesc "Grade Math Olympics data and produce reports"
    <> header "matholympics - grade math olympics exams"
    )

getOptions :: IO Options
getOptions = do
    os <- execParser opts
    let td = texDir os
    unless (null td) $ createDirectoryIfMissing False td 
    return os

getDataFile :: DataFile -> Options -> FilePath
getDataFile df os = dataDir os </> fname df
    where
       fname DataFile = dataFile os
       fname SchoolsFile = schoolCodes os
       fname (MCA One) = l1MCA os
       fname (MCA Two) = l2MCA os

getTeXFile :: TeXPrefix -> Options -> FilePath -> FilePath
getTeXFile tp os level = texDir os </> fname tp
    where
        fname Schools = "Schools" <.> "tex"
        fname pref = show pref <> level <.> "tex"

