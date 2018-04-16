{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib where

import           Prelude             (Bool, Eq, Num, Show (..), fromIntegral,
                                      otherwise, (&&), (<=), (>=))

import           Control.Applicative (Applicative)
import           Control.Category    ((.))
import           Control.Lens        (Index, IxValue, Ixed, ix, makeLenses,
                                      (.~))
import           Data.Function       (($), (&))
import           Data.Functor        (fmap, (<$>))
import           Data.Semigroup      ((<>))
import           GHC.Word            (Word8)

import           Data.List           (replicate)
import           Data.String         (unlines, unwords)

import RayCaster

data SqType
  = Wall
  | Floor
  | Player
  deriving Eq

instance Show SqType where
  show Wall   = "="
  show Floor  = "."
  show Player = "o"

data Sqr = Sqr
  { _sqType :: SqType
  , _sqSide :: Word8
  }
  deriving Eq
makeLenses ''Sqr

instance Show Sqr where
  show (Sqr t _) = show t

newtype Room = R
  { unRoom :: [[Sqr]]
  }

instance Show Room where
  show = unlines
    . fmap unwords
    . (fmap . fmap) show
    . unRoom

inBnds :: (Word8, Word8) -> Bool
inBnds (x,y) = b x && b y
  where
    b a = a >= 1 && a <= 7

room :: Room
room = R $ cap : replicate 6 inner <> [cap]
  where
    w = Sqr Wall 64

    inner = w : replicate 6 (Sqr Floor 64) <> [w]

    cap = replicate 8 w

ppos :: (Word8, Word8)
ppos = (4, 4)

w8ix
  :: ( Applicative f
     , Num (Index m)
     , Ixed m
     )
  => Word8
  -> (IxValue m -> f (IxValue m))
  -> m
  -> f m
w8ix w = ix (fromIntegral w)

setPos :: SqType -> (Word8, Word8) -> Room -> Room
setPos st p@(c,r) (R rr)
  | inBnds p = R (rr & w8ix c . w8ix r . sqType .~ st)
  | otherwise = R rr
