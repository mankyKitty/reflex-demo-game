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

{-
Finding the coordinate of A.
 If the ray is facing up
   A.y = rounded_down(Py/64) * (64) - 1;
 If the ray is facing down
   A.y = rounded_down(Py/64) * (64) + 64;
-}
getY
  :: (Int -> b)
  -> Lens' (V2 Double) Double
  -> (Double -> Double -> Double)
  -> P
  -> Sqr
  -> b
getY cs l fn p s = cs . floor $ fn
  (p ^. playerPosition . l)
  (s ^. sqSide . to fromIntegral)

rayFunction
  :: Dir
  -> (Double -> Double -> Double)
rayFunction d pxy gsize
  | d `elem` [U, L] = pxy / gsize * gsize - 1
  | d `elem` [D, R] = pxy / gsize * gsize + gsize

horizontalFirstY
  :: Ray
  -> P
  -> Sqr
  -> HorizAY
horizontalFirstY f =
  getY HorizAY _y (rayFunction . fst $ rayDir f)

-- A.x = Px + (Py-A.y)/tan(ALPHA);
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
    HorizAX . floor $

    x + (y - fromIntegral ay)
      /
    tan ((pi / 180) * alpha)

verticalFirstX
  :: Ray
  -> P
  -> Sqr
  -> VertAX
verticalFirstX f =
  getY VertAX _x (rayFunction . snd $ rayDir f)

-- A.y = Py + (Px-A.x)*tan(ALPHA);
verticalFirstY
  :: P
  -> VertAX
  -> Ray
  -> VertAY
verticalFirstY p (VertAX ay) (Ray alpha) =
  let
    x = p ^. playerPosition . _x
    y = p ^. playerPosition . _y
  in
    VertAY . floor $ y + (x - fromIntegral ay)
      /
    tan ((pi / 180) * alpha)

{-
Note: Ya,Xa is just the height/width of the grid.

) Finding Ya
   If the ray is facing up, it will be negative.
     Ya=-64;
   If the ray is facing down, it will be positive.
     Ya=64;
) Find Xa
   If the ray is facing right, it will be positive
     Xa=64;
   If the ray is facing left, it will be negative.
     Xa=-64;
-}
directionStepSize
  :: (Int -> b)
  -> ((Dir,Dir) -> Dir)
  -> Ray
  -> Sqr
  -> b
directionStepSize mkB dirFn f s = mkB . fromIntegral $ case dirFn (rayDir f) of
  U -> s ^. sqSide . to negate
  L -> s ^. sqSide . to negate
  D -> s ^. sqSide
  R -> s ^. sqSide

horizontalIntersectionStepSize :: Ray -> Sqr -> HorizStep
horizontalIntersectionStepSize = directionStepSize HorizStep fst

verticalIntersectionStepSize :: Ray -> Sqr -> VertStep
verticalIntersectionStepSize = directionStepSize VertStep snd

{-
) Finding Xa (Horizontal Intersection)
   Y/Xa = GRID_SIZE/tan(ANGLE) = STEP;
--
   Y/Xa = 64/tan(60) = 36;
-}
stepSize
  :: Ray
  -> Sqr
  -> StepSize
stepSize (Ray a) s = StepSize
  . floor
  $ (s ^. sqSide . to fromIntegral) / tan ((pi / 180) * a)

stepHorizontalIntersection
  :: V2 Int
  -> Sqr
  -> HorizStep
  -> V2 Int
stepHorizontalIntersection v s (HorizStep sz) = v
  & _x +~ sz
  & _y +~ (s ^. sqSide . to fromIntegral)

stepVerticalIntersection
  :: V2 Int
  -> Sqr
  -> VertStep
  -> V2 Int
stepVerticalIntersection v s (VertStep sz) = v
  & _x +~ (s ^. sqSide . to fromIntegral)
  & _y +~ sz

toRoomCoord
  :: Integral a
  => Sqr
  -> a
  -> a
  -> V2 a
toRoomCoord s x y = V2 (f x) (f y)
  where f v = s ^. sqSide . to (div v. fromIntegral)

toTheWall
  :: (V2 Int -> Sqr -> t -> V2 Int)
  -> t
  -> Room
  -> Sqr
  -> V2 Int
  -> Maybe (V2 Double, Sqr)
toTheWall stepFn dirStep rm s v@(V2 x y) =
  case atPos (toRoomCoord s x y) rm of
    Just sq@Sqr { _sqType = Wall } -> Just (V2 (fromIntegral x) (fromIntegral y), sq)
    Just _                         -> toTheWall stepFn dirStep rm s (stepFn v s dirStep)
    Nothing                        -> Nothing

distanceTo
  :: P
  -> Maybe (V2 Double, Sqr)
  -> Distance
distanceTo p =
  Distance . maybe 0 (LM.distance (p^.playerPosition) . fst)

castSingleRay
  :: Room
  -> Sqr
  -> Ray
  -> RayBeta
  -> P
  -> Maybe RayCast
castSingleRay rm s ray rayBeta p =
  let
    (HorizAY haY1) = horizontalFirstY ray p s
    (HorizAX haX1) = horizontalFirstX p (HorizAY haY1) ray

    (VertAX vaX1) = verticalFirstX ray p s
    (VertAY vaY1) = verticalFirstY p (VertAX vaX1) ray

    mHWallPos = toTheWall stepHorizontalIntersection (horizontalIntersectionStepSize ray s) rm s (V2 haX1 haY1)
    mVWallPos = toTheWall stepVerticalIntersection (verticalIntersectionStepSize ray s) rm s (V2 vaX1 vaY1)

    hDist = distanceTo p mHWallPos
    vDist = distanceTo p mVWallPos
  in
    if hDist > vDist
    then uncurry (RayCast $ correctDistance vDist rayBeta) <$> mVWallPos
    else uncurry (RayCast $ correctDistance hDist rayBeta) <$> mHWallPos

correctDistance
  :: Distance
  -> RayBeta
  -> Distance
correctDistance (Distance d) (RayBeta rb) =
  Distance $ d * cos ((pi / 180) * rb)
