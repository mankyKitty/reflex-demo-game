{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RayWut where

import           Prelude             (Bool, Double, Eq, Int, Num, Show (..), cos, sin, negate,
                                      floor, fromIntegral, otherwise, pi, tan, ceiling,
                                      undefined, (&&), (*), (/), (<=), (>=), (+), (-))

import           Control.Applicative (Applicative)
import           Control.Category    ((.))
import           Control.Lens        (Index, IxValue, Ixed, ix, to, (.~), (^.), (%~),
                                      _Wrapped)
import           Data.Function       (($), (&))
import           Data.Functor        (fmap, (<$>))
import           Data.Semigroup      ((<>))
import           GHC.Word            (Word8)

import           Data.List           (replicate)
import           Data.String         (unlines, unwords)

import Linear.V3 (V3 (V3), _x,_y)
import Linear.Matrix (M22,M33,M44,identity)
import Linear.V2 (V2 (V2))
import Linear.V4 (V4 (V4), _w)

import           RayCaster
import           Types

-- $setup
-- >>> import Linear.V2 (V2 (V2))
-- >>> let sqrSize = (64 :: Int)
-- >>> let fov = mkFov (Height 200) (Width 320) (Angle 60)
-- >>> let rCast = RayCast (Distance 330) (Sqr Wall 64)

rotationMatrix
  :: P
  -> M22 Double
rotationMatrix p =
  let
    rads = p ^. playerFacing . to toRadians
  in
    V2 (V2 (cos rads) (negate (sin rads)))
       (V2 (sin rads) (cos rads))

-- >>> projectedSliceHeight sqrSize rCast fov
-- 54.0
projectedSliceHeight
  :: Int
  -> RayCast
  -> FOV
  -> Double
projectedSliceHeight s rc fov =
  let
    d = rc ^. rayCastDistance . _Wrapped
    pd = fov ^. fovDistance . _Wrapped
  in
    fromIntegral . ceiling $ fromIntegral s / d * pd

-- |
-- >>> mkFov (Height 200) (Width 320) (Angle 60)
-- FOV {_fovHeight = Height 200, _fovWidth = Width 320, _fovDistance = Distance 277.0, _fovAngle = Angle {_unAngle = 60.0}}
mkFov :: Height -> Width -> Angle -> FOV
mkFov h w a = FOV h w (calcFovDistance w a) a

-- |
-- >>> calcFovDistance (Width 320) (Angle 60)
-- Distance 277.0
--
calcFovDistance
  :: Width
  -> Angle
  -> Distance
calcFovDistance (Width w) (Angle a) =
  let
    w' = fromIntegral w
  in
    Distance . fromIntegral . floor $ (w' / 2) / tan (toRadians . Angle $ (a / 2))

inBnds :: (Word8, Word8) -> Bool
inBnds (x,y) = b x && b y
  where b a = a >= 1 && a <= 7

setPos :: SqType -> (Word8, Word8) -> Room -> Room
setPos st p@(c,r) (Room rr)
  | inBnds p = Room (rr & w8ix c . w8ix r . sqType .~ st)
  | otherwise = Room rr
  where w8ix w = ix (fromIntegral w)

