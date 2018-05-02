{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Main (main) where

import Control.Applicative ((<|>))

import           Control.Lens                     (to, (+~), (.~), (^.), (%~), imap,
                                                   _Wrapped)
import           Control.Monad                    (void)

import qualified Data.List as L

import           Data.Foldable                    (foldlM, traverse_)
import           Data.Function                    ((&))
import           Data.Functor                     (($>), (<$))
import           Data.Maybe                       (catMaybes,mapMaybe)

import Linear.Matrix (identity, M44, (!*))
import Linear.V4 (_w)
import           Linear.V2                        (V2 (..), _x, _y)

import           Language.Javascript.JSaddle.Warp (run)

import qualified Reflex                           as R
import           Reflex.Dom                       (MonadWidget, (=:))
import qualified Reflex.Dom                       as RD
import           Reflex.Dom.Core                  (mainWidget)

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Map                         (Map)
import qualified Data.Map                         as Map

import           JSDOM.CanvasRenderingContext2D   (CanvasRenderingContext2D)
import qualified JSDOM.CanvasRenderingContext2D   as C
import qualified JSDOM.CanvasPath as C

import           JSDOM.Types                      (JSM, JSString, liftJSM)

-- Dude, seriously?! Fix this up, what even are these modules?
import qualified Reflex.Dom.CanvasBuilder.Types   as CD
import qualified Reflex.Dom.CanvasDyn             as CD

import           Control.Monad.State              (MonadState, get,put)

import           DrawM                            (DrawM, runDrawM)

import qualified RayCaster as R
import           RayCaster                        (castSingleRay)
import qualified RayWut as R

import           Types                            (Angle (..), FOV (..),
                                                   Height (..), Ray (Ray),
                                                   RayBeta (RayBeta), RayCast,
                                                   Room (..), SqType (..),
                                                   Sqr (..), Width (..))

import qualified Types                            as T

renderRayCast
  :: CanvasRenderingContext2D
  -> FOV
  -> Int
  -> Int
  -> RayCast
  -> JSM ()
renderRayCast cx fov s rIth rc = do
  let
    w = fov ^. T.fovWidth . _Wrapped . to fromIntegral
    h = fov ^. T.fovHeight . _Wrapped . to fromIntegral

    sliceHeight = R.projectedSliceHeight s rc fov

    x = (negate w/2) + (fromIntegral rIth)
    y = negate (sliceHeight/2)

  C.moveTo cx (realToFrac x) (realToFrac y)
  C.lineTo cx (realToFrac x) (realToFrac $ y + sliceHeight)

screenWidth :: Double
screenWidth = 320

screenHeight :: Double
screenHeight = 240

sqrSize :: Int
sqrSize = 64

fovAngle :: Double
fovAngle = 60

angleBetweenRays :: Double
angleBetweenRays = fovAngle / screenWidth

halfFOV :: T.Angle
halfFOV = T.Angle (fovAngle / 2)

rayList :: [Double]
rayList = (angleBetweenRays *) <$> [0.. (screenWidth - 1)]

betaRays :: [RayBeta]
betaRays =
  let
    halfFOV' = _unAngle halfFOV
    step = halfFOV' - angleBetweenRays
  in
    [ RayBeta rb | rb <- [halfFOV', step .. (negate halfFOV')] ]

player :: T.P
player = T.P (V2 p (p - s)) (Angle fovAngle)
  where
    s = fromIntegral sqrSize
    p = s * 3 + 32

fov :: T.FOV
fov = R.mkFov
  (Height (floor screenHeight))
  (Width (floor screenWidth))
  (Angle fovAngle)

canvasAttrs :: Map Text Text
canvasAttrs = Map.fromList
  [ ("width", "512")
  , ("height", "512")
  ]

cameraAttrs :: Map Text Text
cameraAttrs = Map.fromList
  [ ("width", "320")
  , ("height", "240")
  -- , ("style", "transform-origin: 0 0;transform: scale(1.5, 1.5);")
  ]

stepSquares
  :: Room
  -> Int
  -> (V2 Int -> Int -> Ray -> V2 Int)
  -> Ray
  -> V2 Int
  -> Maybe ((V2 Int,Sqr), V2 Int)
stepSquares rm sqrS fn ray v@(V2 x y) =
  (\s -> ((v,s), fn v sqrS ray)) <$> T.atPos (R.toRoomCoord sqrS x y) rm

mkHorizInters, mkVertInters :: Ray -> T.P -> [(V2 Int, T.Sqr)]
mkHorizInters r p =
  let (T.HorizAX hx, T.HorizAY hy) = R.firstHorizontalIntersection r p sqrSize
  in L.unfoldr (stepSquares T.room1 sqrSize R.stepHorizontalIntersection r) (V2 hx hy)
mkVertInters r p =
  let (T.VertAX vx, T.VertAY vy) = R.firstVerticalIntersection r p sqrSize
  in L.unfoldr (stepSquares T.room1 sqrSize R.stepVerticalIntersection r) (V2 vx vy)

drawInters
  :: CanvasRenderingContext2D
  -> V2 Int
  -> JSM ()
drawInters cx (V2 x y) =
  C.fillRect cx (fromIntegral x) (fromIntegral y) 10 10

renderMap p firstRay lastRay cx _ = do
  renderRoom T.room1 cx
  renderPlayer p cx

  C.setFillStyle cx ("blue" :: JSString)
  traverse_ (drawInters cx . fst) (mkHorizInters firstRay p)
  C.setFillStyle cx ("green" :: JSString)
  traverse_ (drawInters cx . fst) (mkVertInters firstRay p)

  C.setFillStyle cx ("blue" :: JSString)
  traverse_ (drawInters cx . fst) (mkHorizInters lastRay p)
  C.setFillStyle cx ("green" :: JSString)
  traverse_ (drawInters cx . fst) (mkVertInters lastRay p)

app :: MonadWidget t m => m ()
app = do
  let
  (wrapperEle, (innerEle, _)) <- RD.elAttr' "div" ("tabindex" =: "0") $
    RD.elAttr' "canvas" cameraAttrs RD.blank

  dCamCx <- (fmap . fmap) CD._canvasInfo_context $
    CD.dContext2d (CD.CanvasConfig innerEle mempty)

  (e, _) <- RD.elAttr' "canvas" canvasAttrs RD.blank
  eDraw <- RD.button "Go"

  dCx <- fmap CD._canvasInfo_context
    <$> CD.dContext2d (CD.CanvasConfig e mempty)

  let
    rayCast p rIth rayAngle =
      let
        rA = p ^. T.playerFacing . to (R.createRay halfFOV rayAngle)

        -- where RayBeta is the angle of the ray that is being cast relative to
        -- the viewing angle. I can get away with this because we generate the
        -- list of betaRays beforehand and we have a known quantity based on the `rayList`
        rB = betaRays !! rIth
      in
        (castSingleRay T.room1 sqrSize rA rB p, rIth)

    buildRays ply = rayCast ply `imap` rayList

    rays cx rs = do
      C.beginPath cx
      traverse_ (\(rc,i) -> renderRayCast cx fov sqrSize i rc) rs
      C.closePath cx
      C.stroke cx

    camRender
      :: [(RayCast, Int)]
      -> CanvasRenderingContext2D
      -> Double
      -> JSM ()
    camRender r cx _ = do
      C.clearRect cx 0 0 (realToFrac screenWidth) (realToFrac screenHeight)
      C.save cx
      C.setFillStyle cx ("lightblue" :: JSString)
      C.fillRect cx 0 0 (realToFrac screenWidth) (realToFrac screenHeight)
      C.setFillStyle cx ("darkgrey" :: JSString)
      C.translate cx (realToFrac $ screenWidth / 2) (realToFrac $ screenHeight / 2)
      rays cx r
      C.restore cx

  eLeftBtn <- RD.button "Turn left a bit"
  eRightBtn <- RD.button "Turn right a bit"

  let
    -- Rotation based on Ray Casting 0 at E, 90 at N, 180 at W, 270 at S
    rotRight a = T.playerFacing %~ (`T.subtractAngle` a)
    rotLeft a = T.playerFacing %~ T.addAngle a

    eLeft = RD.keypress RD.ArrowLeft wrapperEle <> eLeftBtn
    eRight = RD.keypress RD.ArrowRight wrapperEle <> eRightBtn

    playerSpeed = 10

    moveForward p =
      R.movePlayer p T.Forward playerSpeed

    moveBackward p =
      R.movePlayer p T.Backward playerSpeed

    eForward = RD.keypress RD.ArrowUp wrapperEle
    eBackward = RD.keypress RD.ArrowDown wrapperEle

  dPlayer <- R.foldDyn ($) player $ R.mergeWith (.)
    [ rotLeft (Angle 2) <$ eLeft
    , rotRight (Angle 2) <$ eRight
    , moveForward <$ eForward
    , moveBackward <$ eBackward
    ]

  let
    dFirstRay = (^. T.playerFacing . to (Ray . T._unAngle . (`T.subtractAngle` halfFOV)) ) <$> dPlayer
    dLastRay = (^. T.playerFacing . to (Ray . T._unAngle . T.addAngle halfFOV) ) <$> dPlayer

    dFirstHI = mkHorizInters <$> dFirstRay <*> dPlayer
    dFirstVI = mkVertInters <$> dFirstRay <*> dPlayer

    dLastHI = mkHorizInters <$> dLastRay <*> dPlayer
    dLastVI = mkVertInters <$> dLastRay <*> dPlayer

  dRays <- R.holdDyn (buildRays player) $
    buildRays <$> R.updated dPlayer

  let
    eMoved = eDraw <> (() <$ R.updated dPlayer)

  eRendered <- CD.drawWithCx dCx (renderMap <$> dPlayer <*> dFirstRay <*> dLastRay) eMoved
  eRendered' <- CD.drawWithCx dCamCx (camRender <$> dRays) eMoved

  dRendered <- R.holdDyn "Not Rendered" $
    "Rendered!" <$ eRendered

  RD.divClass "DEBUG" $
    RD.display dRendered

  RD.divClass "DEBUG" $
    RD.display dPlayer

renderSqr
  :: V2 Double
  -> Sqr
  -> CanvasRenderingContext2D
  -> JSM ()
renderSqr pos sq cx = do
  C.setFillStyle cx sqColour
  C.fillRect cx (p _x) (p _y) sqS sqS
  C.setStrokeStyle cx ("black" :: JSString)
  C.strokeRect cx (p _x) (p _y) sqS sqS
  where
    sqS = fromIntegral sqrSize
    p l = pos ^. l . to realToFrac

    sqColour :: JSString
    sqColour = case _sqType sq of
      Wall -> "darkgrey"
      Floor -> "aqua"

renderPlayer :: T.P -> CanvasRenderingContext2D -> JSM ()
renderPlayer (T.P pos face) cx = do
  C.save cx

  C.translate cx (pos ^. _x . to realToFrac) (pos ^. _y . to realToFrac)
  let
    faceRadians = realToFrac $ T.toRadians face
    rCos = cos faceRadians
    rSin = sin faceRadians

  C.rotate cx (-faceRadians)

  C.setStrokeStyle cx ("red" :: JSString)
  C.strokeRect cx 0 0 10 10

  C.setLineWidth cx 5

  C.beginPath cx
  C.moveTo cx 0 0
  C.lineTo cx 100 0
  C.closePath cx
  C.stroke cx
  C.restore cx

renderRoom :: Room -> CanvasRenderingContext2D -> JSM ()
renderRoom (Room rm) cx =
  let
    o = V2 0.0 0.0
    xStep = V2 (fromIntegral sqrSize) 0.0
    yStep = V2 0.0  (fromIntegral sqrSize)

    rSqrs sq offs =
      renderSqr offs sq cx $> offs
  in
    --- Hrmmm
    void $ foldlM
      (\o' sqs ->
          yStep + o' <$ foldlM
          (\o'' s ->
              (xStep +) <$> rSqrs s o''
          ) o' sqs
      ) o rm

main :: IO ()
main = run 3911 $ mainWidget app
