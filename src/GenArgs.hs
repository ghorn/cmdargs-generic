{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GenArgs
       ( Flag(..), FlagInfo(..), Parsed(..), ToFlags(..)
       , parseArgs
       ) where

import GHC.Generics

import Control.Lens hiding (to, from)
import qualified Data.Foldable as F
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified System.Console.CmdArgs.Explicit as CA
import Text.Read ( readMaybe )

data FlagInfo a =
  Required
  | Optional
  | Default String
  deriving Show

data Flag a =
  Flag
  { fInfo :: FlagInfo a
  , fValue :: a
  , fNames :: [String]
  , fType :: CA.FlagHelp
  , fHelp :: CA.Help
  } deriving Show -- Functor

data Parsed a =
  Parsed
  { unParsed :: a
  } deriving Show -- Functor

parseArgs :: forall f . ToFlags f => f Flag -> String -> String -> IO (f Parsed)
parseArgs flags0 name help = CA.processArgs mode
  where
    mode = CA.mode name default' help emptyArg flags
    (flags, default') = toFlags flags0
    emptyArg :: CA.Arg (f Parsed)
    emptyArg =
      CA.Arg
      { CA.argValue = const Right
      , CA.argType = "DAMMIT"
      , CA.argRequire = False
      }

class ToFlags f where
  toFlags :: f Flag -> ([CA.Flag (f Parsed)], f Parsed)

  default toFlags ::
    ( Generic (f Flag), Generic (f Parsed)
    , GToFlags (Rep (f Flag)) (Rep (f Parsed))
    ) => f Flag -> ([CA.Flag (f Parsed)], f Parsed)
  toFlags x = (F.toList flags, to default')
    where
      default' :: Rep (f Parsed) p
      flags :: S.Seq (CA.Flag (f Parsed))
      (flags, default') = gtoFlags repLens (from x)
      repLens :: Lens' (f Parsed) (Rep (f Parsed) p)
      repLens f y = fmap to (f (from y))

class GToFlags f g where
  gtoFlags :: Lens' (a Parsed) (g p) -> f p -> (Seq (CA.Flag (a Parsed)), g p)

--instance Read a => GToFlags (Rec0 (Flag a)) (Rec0 (Parsed a)) where
--  gtoFlags = handleField readMaybe

instance GToFlags (Rec0 (Flag String)) (Rec0 (Parsed String)) where
  gtoFlags = handleField Just

instance GToFlags (Rec0 (Flag Int)) (Rec0 (Parsed Int)) where
  gtoFlags = handleField readMaybe

instance GToFlags (Rec0 (Flag Bool)) (Rec0 (Parsed Bool)) where
  gtoFlags = handleField CA.parseBool

handleField ::
  forall a b p
  . (String -> Maybe b) -> Lens' a (Rec0 (Parsed b) p) -> Rec0 (Flag b) p
  -> (Seq (CA.Flag a), Rec0 (Parsed b) p)
handleField readMaybe' lens0 (K1 userFlag) = (S.singleton flag, default')
  where
    default' :: Rec0 (Parsed b) p
    default' = K1 (Parsed (fValue userFlag))

    flag :: CA.Flag a
    flag =
      CA.Flag
      { CA.flagNames = fNames userFlag
      , CA.flagInfo = case fInfo userFlag of
          Required -> CA.FlagReq
          Default x -> CA.FlagOpt x
--          Default _ -> CA.Flag
      , CA.flagValue = update
      , CA.flagType = fType userFlag
      , CA.flagHelp = fHelp userFlag
      }

    update str x0 = case readMaybe' str of
      Nothing -> Left $ "Read error on Int: couldn't read " ++ show str
      Just val -> Right $ (lens0 . rec0Lens .~ val) x0

    rec0Lens :: Lens' (Rec0 (Parsed b) p) b
    rec0Lens f y = fmap (K1 . Parsed) (f (unParsed (unK1 y)))


instance (GToFlags fx gx, GToFlags fy gy)
         => GToFlags ((:*:) fx fy) ((:*:) gx gy) where
  gtoFlags lens0 (fx :*: fy) = (lflags S.>< rflags, ldefault :*: rdefault)
    where
      (lflags, ldefault) = gtoFlags (lens0 . leftLens) fx
      (rflags, rdefault) = gtoFlags (lens0 . rightLens) fy

      leftLens ::  Lens' ((f :*: g) a) (f a)
      leftLens  f (x :*: y) = fmap (\x' -> x' :*: y ) (f x)
      rightLens :: Lens' ((f :*: g) a) (g a)
      rightLens f (x :*: y) = fmap (\y' -> x  :*: y') (f y)

instance GToFlags f g => GToFlags (M1 i c f) (M1 i c g) where
  gtoFlags lens0 (M1 x) = (flags, M1 default')
    where
      (flags, default') = gtoFlags (lens0 . m1Lens) x

--      m1Lens :: Lens' (S1 s f p) (f p)
      m1Lens f y = fmap M1 (f (unM1 y))
