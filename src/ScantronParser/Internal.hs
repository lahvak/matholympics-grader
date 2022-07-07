{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ScantronParser.Internal
    ( Exam(..)
    , Option(..)
    , decodeRec
    ) where

-- cassava imports
import Data.Csv
  ( FromRecord(parseRecord)
  , FromField(parseField)
  , decode
  , HasHeader(NoHeader)
  , (.!)
  )

-- text
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Char (isSpace)

-- read file
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Set
import Data.Set (Set)
import qualified Data.Set as Set

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Option = A | B | C | D | E | Invalid deriving (Eq, Ord, Show)

data Exam = 
    Exam
        { name :: !Text
          , sid :: !Text
          , answers :: [Set Option]
        }
        deriving (Eq, Show)

instance FromField Option where
    parseField s
      | s == "A" = pure A
      | s == "B" = pure B
      | s == "C" = pure C
      | s == "D" = pure D
      | s == "E" = pure E
      | otherwise = pure Invalid

instance FromField (Set Option) where
    parseField r
      | BS.length s == 0 || s == "BLANK" = pure Set.empty
      | BS.length s == 1 = Set.singleton <$> parseField s
      | BS.head s == '(' && BS.last s == ')' = 
          Set.fromList <$> traverse parseField ( BS.split ',' . BS.init . BS.tail $ s)
      | otherwise = pure $ Set.singleton Invalid
      where s = BS.filter (not . isSpace) r

instance FromRecord Exam where
    parseRecord v
        | length v >= 2 = Exam <$> 
            (Text.strip <$> v .! 0) <*> 
                (Text.takeEnd 7 <$> v .! 1) <*>  -- The scantron software puts about 25 spaces before the ID
                    parseRecord (Vector.take 25 $ Vector.drop 2 v)
        | otherwise = fail $ "Malformed record in the data file: " ++ show v

decodeRec :: Data.ByteString.Lazy.ByteString -> Either String (Vector Exam)
decodeRec = decode NoHeader
