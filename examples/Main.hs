{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( main ) where

import GHC.Generics ( Generic )

import GenArgs

data MyArgs f =
  MyArgs
  { myInt :: f Int
  , myBool :: f Bool
  , myString :: f String
  } deriving Generic

instance ToFlags MyArgs

instance Show (MyArgs Flag) where
  show (MyArgs x y z) = "MyArgs " ++ show (x,y,z)
instance Show (MyArgs Parsed) where
  show (MyArgs x y z) = "MyArgs " ++ show (x,y,z)

dummyArgs :: MyArgs Flag
dummyArgs =
  MyArgs
  { myInt =
    Flag
    { fInfo = Required
    , fNames = []
    , fType = "FILE"
    , fHelp = "the important int"
    , fValue = 42
    }
  , myBool =
    Flag
    { fInfo = Required
    , fNames = []
    , fType = "BOOL"
    , fHelp = "the important bool"
    , fValue = True
    }
  , myString =
    Flag
    { fInfo = Required
    , fNames = []
    , fType = "FILE"
    , fHelp = "the important string"
    , fValue = "42"
    }
  }

main :: IO ()
main = do
  args <- parseArgs dummyArgs "woo" "oh yeah"
  print args

