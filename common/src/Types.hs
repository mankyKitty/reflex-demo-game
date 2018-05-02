{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import GHC.Generics (Generic)
import Control.Monad.Par (NFData)

import           Control.Lens (ix, makeLenses, makeWrapped, to, (^.), (^?))
import           Linear.V2

import           GHC.Word     (Word8)

-- $setup
-- let angleBnd a = a >= 0 && a <= 360

newtype Height = Height Int deriving Show
newtype Width  = Width Int deriving Show

newtype Angle  = Angle
  { _unAngle :: Double
  }
  deriving (Show, Eq, Ord)

instance Bounded Angle where
  minBound = Angle 0
  maxBound = Angle 360

toRadians :: Angle -> Double
toRadians (Angle a) = (pi/180) * a

addAngle :: Angle -> Angle -> Angle
addAngle (Angle a) (Angle b) =
  if 360 >= a + b then Angle (a + b)
  else Angle (a - (360 - b))

subtractAngle :: Angle -> Angle -> Angle
subtractAngle (Angle a) (Angle b) =
  if 0 <= (a - b) then Angle (a - b)
  else Angle (360 + (a - b))

mkAngle :: Double -> Maybe Angle
mkAngle a | a >= 0 && a <= 360 = Just (Angle a)
          | otherwise          = Nothing

data FOV = FOV
  { _fovHeight   :: Height
  , _fovWidth    :: Width
  , _fovDistance :: Distance
  , _fovAngle    :: Angle
  }
  deriving Show

data Move
  = Forward
  | Backward
  deriving (Show, Eq)

data SqType
  = Wall
  | Floor
  deriving (Generic, NFData, Eq,Show)

data Sqr = Sqr
  { _sqType :: SqType
  , _sqSide :: Word8
  }
  deriving (Eq, Generic, NFData, Show)

newtype Room = Room
  { unRoom :: [[Sqr]]
  }

instance Show Room where
  show = unlines . fmap unwords . (fmap . fmap) show . unRoom

atPos :: V2 Int -> Room -> Maybe Sqr
atPos (V2 x y) r = r ^? to unRoom . ix y . ix x

data P = P
  { _playerPosition :: V2 Double
  , _playerFacing   :: Angle
  }
  deriving Show

newtype Ray = Ray
  { unRay :: Double
  }
  deriving Show

mkRay :: Double -> Maybe Ray
mkRay a | a >= 0 && a < 360 = Just (Ray a)
        | otherwise         = Nothing

newtype RayBeta = RayBeta
  { unRayBeta :: Double
  }
  deriving Show

data RayCast = RayCast
  { _rayCastDistance :: Distance
  , _rayCastEndSqr   :: Sqr
  }
  deriving (Show, Eq, Generic, NFData)

data Dir
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

rayDir :: Ray -> (Dir, Dir)
rayDir (Ray r)
  | r >=   0 && r <  90 = (U, R)
  | r >=  90 && r < 180 = (U, L)
  | r >= 180 && r < 270 = (D, L)
  | r >= 270 && r < 360 = (D, R)
  | otherwise           = (U, R)

newtype HorizAY = HorizAY Int deriving Show
newtype HorizAX = HorizAX Int deriving Show

newtype VertAX = VertAX Int deriving Show
newtype VertAY = VertAY Int deriving Show

newtype HorizStep = HorizStep Int deriving Show
newtype VertStep = VertStep Int deriving Show

newtype StepSize = StepSize Int deriving Show

newtype Distance = Distance Double
  deriving (Eq, Ord, Show, Generic, NFData)

makeWrapped ''Distance
makeWrapped ''Height
makeWrapped ''Width
makeWrapped ''Angle

makeLenses ''P
makeLenses ''Sqr
makeLenses ''FOV
makeLenses ''RayCast

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
         , [w,f,f,f,w,w,w,w]
         , [w,w,f,f,f,f,f,w]
         , [w,w,w,w,w,w,w,w]
         ]

player2start :: P
player2start = P (V2 96 224) (Angle 30)

room2 :: Room
room2 =
  let
    w = Sqr Wall 64
    f = Sqr Floor 64
  in
    Room [ [w,w,w,w]
         , [f,f,f,f]
         , [f,f,f,f]
         , [f,f,f,f]
         ]
