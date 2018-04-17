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

mkFov :: Height -> Width -> Angle -> FOV
mkFov h w a = FOV h w (calcFovDistance w a) a

calcFovDistance
  :: Width
  -> Angle
  -> Distance
calcFovDistance (Width w) (Angle a) =
  let
    w' = fromIntegral w
    a' = fromIntegral a
  in
    Distance $ (w' / 2) * tan ((pi / 180) * a')

inBnds :: (Word8, Word8) -> Bool
inBnds (x,y) = b x && b y
  where b a = a >= 1 && a <= 7

room1 :: Room
room1 =
  let
    w = Sqr Wall 64
    f = Sqr Floor 64
  in
    Room [ [w,w,w,w,w,w,w,w]
         , [w,f,w,f,f,f,f,w]
         , [w,f,f,f,f,w,f,w]
         , [w,f,f,f,f,w,f,w]
         , [w,f,f,f,f,f,f,w]
         , [w,f,f,w,w,w,w,w]
         , [w,w,f,f,f,f,f,w]
         , [w,w,w,w,w,w,w,w]
         ]

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
