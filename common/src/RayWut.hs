{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RayWut where

import           Prelude             (Bool, Double, Eq, Int, Num, Show (..), fst,
                                      ceiling, cos, floor, fromIntegral, negate,
                                      otherwise, pi, sin, tan, undefined, (&&),
                                      (*), (+), (-), (/), (<=), (>=))

import           Control.Applicative (Applicative)
import           Control.Category    ((.))
import           Control.Lens        (Index, IxValue, Ixed, ix, to, (%~), (.~), (-~), (+~),
                                      (^.), _Wrapped)
import           Data.Function       (($), (&))
import           Data.Functor        (fmap, (<$>))
import           Data.Semigroup      ((<>))
import           GHC.Word            (Word8)

import           Data.List           (replicate)
import           Data.String         (unlines, unwords)

import           Linear.Matrix       (M22, M33, M44, identity)
import           Linear.V2           (V2 (V2))
import           Linear.V3           (V3 (V3), _x, _y)
import           Linear.V4           (V4 (V4), _w)

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

-- |
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
    Distance . fromIntegral . floor $ (w' / 2) / tan (toRadians $ Angle (a / 2))

movePlayer
  :: P
  -> Move
  -> Double
  -> P
movePlayer p move speed =
  let
    pRads = p ^. playerFacing . to toRadians
    xCos = speed / (1 / cos pRads)
    ySin = speed / (1 / sin pRads)

    facing = p ^. playerFacing . _Wrapped . to (rayDir . Ray)
  in
    case facing of
      (U, L) -> p & playerPosition . _x -~ xCos & playerPosition . _y -~ ySin
      (U, R) -> p & playerPosition . _x -~ xCos & playerPosition . _y +~ ySin
      (D, L) -> p & playerPosition . _x +~ xCos & playerPosition . _y -~ ySin
      (D, R) -> p & playerPosition . _x +~ xCos & playerPosition . _y +~ ySin
