{-# LANGUAGE FlexibleInstances #-}

module Grader
    (
      StudentResult(..)
    , Answer(..)
    , processExams
    ) where

import Grader.Internal
import Schools (Schools)
import ScantronParser (Exam(..))

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

processExams :: Schools -> [Answer] -> [Answer] -> Vector Exam -> IO (Vector StudentResult)
processExams schools corl1 corl2 = fmap (Vector.mapMaybe id) . mapM (processExam schools corl1 corl2)

