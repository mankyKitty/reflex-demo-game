{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Lib where

import           Prelude             (Bool, Double, Eq, Int, Num, Show (..),
                                      floor, fromIntegral, otherwise, pi, tan,
                                      undefined, (&&), (*), (/), (<=), (>=))

import           Control.Applicative (Applicative)
import           Control.Category    ((.))
import           Control.Lens        (Index, IxValue, Ixed, ix, to, (.~), (^.),
                                      _Wrapped)
import           Data.Function       (($), (&))
import           Data.Functor        (fmap, (<$>))
import           Data.Semigroup      ((<>))
import           GHC.Word            (Word8)

import           Data.List           (replicate)
import           Data.String         (unlines, unwords)

import           RayCaster
import           Types

projectedSliceHeight
  :: Sqr
  -> RayCast
  -> FOV
  -> Double
projectedSliceHeight s rc fov =
  let
    d = rc ^. rayCastDistance . _Wrapped
    pd = fov ^. fovDistance . _Wrapped
  in
    (s ^. sqSide . to fromIntegral) / (d * pd)

mkFov :: FOV
mkFov = undefined

calcFovDistance
  :: Double
  -> Double
  -> Int
calcFovDistance w a =
  floor $ w * tan ((pi / 180) * a)

inBnds :: (Word8, Word8) -> Bool
inBnds (x,y) = b x && b y
  where
    b a = a >= 1 && a <= 7

room :: Room
room = Room $ cap : replicate 6 inner <> [cap]
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
setPos st p@(c,r) (Room rr)
  | inBnds p = Room (rr & w8ix c . w8ix r . sqType .~ st)
  | otherwise = Room rr
