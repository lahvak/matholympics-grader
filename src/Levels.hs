module Levels where

data Level = One | Two deriving (Eq, Show)

newtype L1 a = L1 {fromL1 :: a} deriving(Eq, Ord, Show)
newtype L2 a = L2 {fromL2 :: a} deriving(Eq, Ord, Show)
newtype Combined a = Combined {fromCombined :: a} deriving(Eq, Ord, Show)

instance Functor L1 where
    fmap f (L1 a) = L1 $ f a

instance Functor L2 where
    fmap f (L2 a) = L2 $ f a

instance Functor Combined where
    fmap f (Combined a) = Combined $ f a
