module MCAFiles
    ( readMCAFile
    ) where

import MCAFiles.Internal (parseMCAList)

import Grader (Answer)

readMCAFile :: FilePath -> IO (Either String [Answer])
readMCAFile = fmap (parseMCAList . lines) . readFile
