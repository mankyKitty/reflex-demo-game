{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Types where

import           Control.Lens (ix, makeLenses, makeWrapped, to, (^.), (^?))
import           Linear.V2

import           GHC.Word     (Word8)

data FOV = FOV
  { _fovHeight   :: Int
  , _fovWidth    :: Int
  , _fovDistance :: Distance
  , _fovAngle    :: Int
  }

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
  deriving (Eq, Show)

newtype Room = Room
  { unRoom :: [[Sqr]]
  }

instance Show Room where
  show = unlines
    . fmap unwords
    . (fmap . fmap) show
    . unRoom

atPos :: V2 Int -> Room -> Maybe Sqr
atPos (V2 x y) r = r ^? to unRoom . ix x . ix y

data P = P
  { _playerPosition :: V2 Double
  , _playerFacing   :: Double
  }

newtype Ray = Ray
  { unRay :: Double
  }

newtype RayBeta = RayBeta
  { unRayBeta :: Double
  }

data RayCast = RayCast
  { _rayCastDistance :: Distance
  , _rayCastEnd      :: V2 Double
  , _rayCastEndSqr   :: Sqr
  }

data Dir
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

rayDir :: Ray -> (Dir, Dir)
rayDir (Ray r)
  | r > 0   && r < 90 = (U, L)
  | r >= 90 && r < 180 = (U, R)
  | r >= 180 && r < 270 = (D, R)
  | r >= 270 && r < 360 = (D, L)

newtype HorizAY = HorizAY Int deriving Show
newtype HorizAX = HorizAX Int deriving Show

newtype VertAX = VertAX Int deriving Show
newtype VertAY = VertAY Int deriving Show

newtype HorizStep = HorizStep Int deriving Show
newtype VertStep = VertStep Int deriving Show

newtype StepSize = StepSize Int deriving Show

newtype Distance = Distance Double
  deriving (Eq, Ord, Show)

makeWrapped ''Distance

makeLenses ''P
makeLenses ''Sqr
makeLenses ''FOV
makeLenses ''RayCast
