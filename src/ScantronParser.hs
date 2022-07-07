{-# LANGUAGE FlexibleInstances #-}

module ScantronParser
    ( Exam(..)
    , Option(..)
    , decodeFile
    ) where

import ScantronParser.Internal

-- cassava imports
import Data.Csv
  ( decode
  , HasHeader(NoHeader)
  )

-- text
import Data.ByteString.Lazy (readFile)
--
-- vector
import Data.Vector (Vector)

decodeFile :: FilePath -> IO (Either String (Vector Exam))
decodeFile fPath = decode NoHeader <$> Data.ByteString.Lazy.readFile fPath 
