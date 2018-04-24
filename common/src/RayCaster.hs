{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module RayCaster where

import           Control.Applicative (liftA2)
import           Control.Lens        (Lens', each, makeLenses, over, to, view,
                                      (%~), (+~), (^.))

import qualified Linear.Metric       as LM
import           Linear.V2

import           Data.Function       ((&))
import           Data.Maybe          (fromMaybe)

import           GHC.Word            (Word8)

import qualified Data.List.NonEmpty  as NE
import qualified Data.Sequence       as S

import           Types

-- $setup
-- >>> let sqrSize = (64 :: Int)
-- >>> let player = P (V2 96 224) (Angle 60.0)
-- >>> let player2 = P (V2 224 224) (Angle 60.0)
-- >>> let player3 = P (V2 224 224) (Angle 237.0)
-- >>> let ray3 = Ray 207.0

-- |
-- Finding the coordinate of A.
--  If the ray is facing up
--    A.y = rounded_down(Py/64) * (64) - 1;
--  If the ray is facing down
--    A.y = rounded_down(Py/64) * (64) + 64;
--
getFirstIntersection
  :: (Int -> b)
  -> Lens' (V2 Double) Double
  -> (Double -> Double -> Double)
  -> P
  -> Int
  -> b
getFirstIntersection cs l fn p s = cs . floor $ fn
  (p ^. playerPosition . l)
  (fromIntegral s)

rayFunction
  :: Dir
  -> (Double -> Double -> Double)
rayFunction d pxy gsize
  | d `elem` [U, L] = (fromIntegral . floor $ pxy / gsize) * gsize - 1
  | d `elem` [D, R] = (fromIntegral . floor $ pxy / gsize) * gsize + gsize

-- |
-- >>> horizontalFirstY (Ray 60.0) player sqrSize
-- HorizAY 191
-- >>> horizontalFirstY ray3 player3 sqrSize
-- HorizAY 256
horizontalFirstY
  :: Ray
  -> P
  -> Int
  -> HorizAY
horizontalFirstY f =
  getFirstIntersection HorizAY _y (rayFunction . fst $ rayDir f)

-- |
-- >>> horizontalFirstX player (HorizAY 191) (Ray 60.0)
-- HorizAX 115
-- >>> horizontalFirstX player3 (HorizAY 256) ray3
-- HorizAX 161
horizontalFirstX
  :: P
  -> HorizAY
  -> Ray
  -> HorizAX
horizontalFirstX p (HorizAY ay) (Ray alpha) =
  let
    x = p ^. playerPosition . _x
    y = p ^. playerPosition . _y
  in
    HorizAX . floor $ x + (y - fromIntegral ay) / tan (toRadians . Angle $ alpha)

-- |
-- Find coordinate of the first intersection (point B in this example).
-- The ray is facing right in the picture,
--   B.x = rounded_down(Px/64) * (64) + 64.
-- If the ray had been facing left
--   B.x = rounded_down(Px/64) * (64) – 1.
-- |
-- >>> verticalFirstX (Ray 60.0) player sqrSize
-- VertAX 128
-- >>> verticalFirstX (Ray 60.0) player2 sqrSize
-- VertAX 256
-- >>> verticalFirstX (Ray 186.0) player sqrSize
-- VertAX 63
-- >>> verticalFirstX (Ray 120.0) player2 sqrSize
-- VertAX 191
-- >>> verticalFirstX ray3 player3 sqrSize
-- VertAX 191
verticalFirstX
  :: Ray
  -> P
  -> Int
  -> VertAX
verticalFirstX f =
  getFirstIntersection VertAX _x (rayFunction . snd $ rayDir f)

-- |
-- A.y = Py + (Px-A.x)*tan(ALPHA);
--
-- >>> verticalFirstY player (VertAX 128) (Ray 60.0)
-- VertAY 168
-- >>> verticalFirstY player2 (VertAX 256) (Ray 60.0)
-- VertAY 168
-- >>> verticalFirstY player (VertAX 63) (Ray 186.0)
-- VertAY 227
-- >>> verticalFirstY player2 (VertAX 191) (Ray 120.0)
-- VertAY 166
-- >>> verticalFirstY player3 (VertAX 191) ray3
-- VertAY 240
verticalFirstY
  :: P
  -> VertAX
  -> Ray
  -> VertAY
verticalFirstY p (VertAX ax) (Ray alpha) =
  let
    x = p ^. playerPosition . _x
    y = p ^. playerPosition . _y
  in
    VertAY . floor $ y + (x - fromIntegral ax) * tan (toRadians . Angle $ alpha)

-- |
-- Note: Ya,Xa is just the height/width of the grid.
-- ) Finding Ya
--    If the ray is facing up, it will be negative.
--      Ya=-64;
--    If the ray is facing down, it will be positive.
--      Ya=64;
-- ) Find Xa
--    If the ray is facing right, it will be positive
--      Xa=64;
--    If the ray is facing left, it will be negative.
--      Xa=-64;
--
directionStepSize
  :: (Int -> b)
  -> ((Dir,Dir) -> Dir)
  -> Ray
  -> Int
  -> b
directionStepSize mkB dirFn f s = mkB . fromIntegral $
  case dirFn (rayDir f) of
    U -> negate s
    L -> negate s
    D -> s
    R -> s

-- |
-- >>> horizontalIntersectionStepSize (Ray 60.0) sqrSize
-- HorizStep (-64)
-- >>> horizontalIntersectionStepSize ray3 sqrSize
-- HorizStep 64
horizontalIntersectionStepSize :: Ray -> Int -> HorizStep
horizontalIntersectionStepSize = directionStepSize HorizStep fst

-- |
-- >>> verticalIntersectionStepSize (Ray 60.0) sqrSize
-- VertStep 64
-- >>> verticalIntersectionStepSize ray3 sqrSize
-- VertStep (-64)
verticalIntersectionStepSize :: Ray -> Int -> VertStep
verticalIntersectionStepSize = directionStepSize VertStep snd

-- |
-- >>> stepHorizontalIntersection (V2 115 191) sqrSize (Ray 60.0)
-- V2 151 127
-- >>> stepHorizontalIntersection (V2 151 127) sqrSize (Ray 60.0)
-- V2 187 63
-- >>> stepHorizontalIntersection (V2 256 128) sqrSize (Ray 60.0)
-- V2 292 64
-- >>> stepHorizontalIntersection (V2 292 64) sqrSize (Ray 60.0)
-- V2 328 0
-- >>> stepHorizontalIntersection (V2 161 256) sqrSize ray3
-- V2 36 320
-- >>> stepHorizontalIntersection (V2 36 320) sqrSize ray3
-- V2 (-89) 384
stepHorizontalIntersection
  :: V2 Int
  -> Int
  -> Ray
  -> V2 Int
stepHorizontalIntersection v s ray@(Ray a) =
  let
    d = snd . rayDir $ ray

    floop n
      | n < 0 && d == R = negate n
      | n < 0 && d == L = n
      | d == L          = negate n
      | otherwise       = n

    (HorizStep ya) = horizontalIntersectionStepSize ray s
    xa = floop . floor $ (fromIntegral s) / tan (toRadians . Angle $ a)
  in
  v & _x +~ xa
    & _y +~ ya

-- |
-- >>> stepVerticalIntersection (V2 128 205) sqrSize (Ray 60.0)
-- V2 192 95
-- >>> stepVerticalIntersection (V2 191 95) sqrSize ray3
-- V2 127 127
-- >>> stepVerticalIntersection (V2 127 127) sqrSize ray3
-- V2 63 159
-- >>> stepVerticalIntersection (V2 63 159) sqrSize ray3
-- V2 (-1) 191
-- >>> stepVerticalIntersection (V2 191 4) sqrSize (Ray 102.0)
-- V2 127 (-298)
stepVerticalIntersection
  :: V2 Int
  -> Int
  -> Ray
  -> V2 Int
stepVerticalIntersection v s r@(Ray a) =
  let
    d = fst . rayDir $ r

    floop n
      | n < 0 && d == D = negate n
      | n < 0 && d == U = n
      | d == U          = negate n
      | otherwise       = n

    ya = floop . floor $ (fromIntegral s) * tan (toRadians . Angle $ a)
    (VertStep xa) = verticalIntersectionStepSize r s
  in
  v & _x +~ xa
    & _y +~ ya

-- |
-- >>> toRoomCoord sqrSize 115 191
-- V2 1 2
-- >>> toRoomCoord sqrSize 187 63
-- V2 2 0
toRoomCoord
  :: Int
  -> Int
  -> Int
  -> V2 Int
toRoomCoord s x y =
  V2 (x `div` s) (y `div` s)

toTheWall
  :: (V2 Int -> Int -> Ray -> V2 Int)
  -> Ray
  -> Room
  -> Int
  -> V2 Int
  -> Maybe (V2 Double, Sqr)
toTheWall stepFn ray rm s v@(V2 x y) =
  case atPos (toRoomCoord s x y) rm of
    Just sq@Sqr { _sqType = Wall } -> Just (V2 (fromIntegral x) (fromIntegral y), sq)
    Just _                         -> toTheWall stepFn ray rm s (stepFn v s ray)
    Nothing                        -> Nothing
    

-- |
-- >>> distanceTo player (Just (V2 187 63, Sqr Wall 64))
-- Distance 184.93782739072068
--
distanceTo
  :: P
  -> Maybe (V2 Double, Sqr)
  -> Distance
distanceTo _ Nothing       = Distance 99999
distanceTo p (Just (d, _)) = Distance $ LM.distance (p^.playerPosition) d

-- |
-- >>> firstHorizontalIntersection (Ray 60.0) player sqrSize
-- (HorizAX 115,HorizAY 191)
--
firstHorizontalIntersection
  :: Ray
  -> P
  -> Int
  -> (HorizAX, HorizAY)
firstHorizontalIntersection ray p s =
  let
    hy = horizontalFirstY ray p s
    hx = horizontalFirstX p hy ray
  in
    (hx, hy)

-- |
-- >>> firstVerticalIntersection (Ray 60.0) player sqrSize
-- (VertAX 128,VertAY 168)
--
firstVerticalIntersection
  :: Ray
  -> P
  -> Int
  -> (VertAX, VertAY)
firstVerticalIntersection ray p s =
  let
    vx = verticalFirstX ray p s
    vy = verticalFirstY p vx ray
  in
    (vx, vy)

-- |
-- >>> horizontalToWall (Ray 60.0) room1 64 (V2 256 128)
-- Just (V2 328.0 0.0,Sqr {_sqType = Wall, _sqSide = 64})
horizontalToWall
  :: Ray
  -> Room
  -> Int
  -> V2 Int
  -> Maybe (V2 Double, Sqr)
horizontalToWall ray rm s v2h =
  toTheWall stepHorizontalIntersection ray rm s v2h

-- >>> verticalToWall (Ray 60.0) room1 64 (V2 128 205)
-- Just (V2 566.0 48.0,Sqr {_sqType = Wall, _sqSide = 64})
verticalToWall
  :: Ray
  -> Room
  -> Int
  -> V2 Int
  -> Maybe (V2 Double, Sqr)
verticalToWall ray rm s v2v =
  toTheWall stepVerticalIntersection ray rm s v2v

finaliseRay
  :: P
  -> RayBeta
  -> Int
  -> Maybe (V2 Double, Sqr)
  -> RayCast
finaliseRay player rbeta sqrSize mRayInter =
  let
    finalDist = correctDistance (distanceTo player mRayInter) rbeta
    def = RayCast finalDist (Sqr Wall (fromIntegral sqrSize))
  in
    maybe def (RayCast finalDist . snd) mRayInter

castSingleRay
  :: Room
  -> Int
  -> Ray
  -> RayBeta
  -> P
  -> RayCast
castSingleRay rm s ray rayBeta p =
  let
    (HorizAX haX1, HorizAY haY1) = firstHorizontalIntersection ray p s
    (VertAX vaX1, VertAY vaY1) = firstVerticalIntersection ray p s

    mHWallPos = horizontalToWall ray rm s (V2 haX1 haY1)
    mVWallPos = verticalToWall ray rm s (V2 vaX1 vaY1)

    hDist = distanceTo p mHWallPos
    vDist = distanceTo p mVWallPos
  in
    finaliseRay p rayBeta s $
      if hDist > vDist then mVWallPos else mHWallPos

correctDistance
  :: Distance
  -> RayBeta
  -> Distance
correctDistance (Distance wonky) (RayBeta rb) =
  Distance $ wonky * cos (toRadians . Angle $ rb)
