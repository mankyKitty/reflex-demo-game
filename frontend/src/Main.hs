{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Main (main) where

import qualified Control.Monad.Par                as Par
import qualified Control.Monad.Par.Combinator     as Par

import           Control.Lens                     (to, (+~), (.~), (^.), (%~),
                                                   _Wrapped)
import           Control.Monad                    (void)

import qualified Data.List as L

import           Data.Foldable                    (foldlM, traverse_)
import           Data.Function                    ((&))
import           Data.Functor                     (($>), (<$))
import           Data.Maybe                       (catMaybes,mapMaybe)

import           Linear.V2                        (V2 (..), _x, _y)

import           Language.Javascript.JSaddle.Warp (run)

import qualified Reflex                           as R
import           Reflex.Dom                       (MonadWidget)
import qualified Reflex.Dom                       as RD
import           Reflex.Dom.Core                  (mainWidget)

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
import           RayWut                           (mkFov, projectedSliceHeight)

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
  -> Double
  -> RayCast
  -> JSM ()
renderRayCast cx fov s rIth rc = do
  let
    w = fov ^. T.fovWidth . _Wrapped . to fromIntegral
    h = fov ^. T.fovHeight . _Wrapped . to fromIntegral

    sliceHeight = projectedSliceHeight s rc fov

    x = (negate w/2) + rIth
    y = negate (sliceHeight/2)

  C.moveTo cx (realToFrac x) (realToFrac y)
  C.lineTo cx (realToFrac x) (realToFrac $ y + sliceHeight)

data MvAngle
  = Incr
  | Decr

mvAngle :: MvAngle -> Double -> Double
mvAngle Decr a | a == 359.0 = 0.0
               | otherwise = a + 1
mvAngle Incr a | a == 0.0 = 359.0
               | otherwise = a - 1

app :: MonadWidget t m => m ()
app = do
  let
    angleBetweenRays :: Double
    angleBetweenRays = 60/320

    halfFOV :: T.Angle
    halfFOV = T.Angle 30

    sqrSize :: Int
    sqrSize = 64

    p :: Double
    p = fromIntegral sqrSize * 3 + 32

    player = T.P (V2 p (p - fromIntegral sqrSize)) (Angle 60)

    canvasAttrs = Map.fromList
      [ ("width", "512")
      , ("height", "512")
      ]

    cameraAttrs = Map.fromList
      [ ("width", "320")
      , ("height", "200")
      ]

  (outerEle, (innerEle, _)) <- RD.el' "game-screen" $
    RD.elAttr' "canvas" cameraAttrs RD.blank

  dCamCx <- (fmap . fmap) CD._canvasInfo_context $
    CD.dContext2d (CD.CanvasConfig innerEle mempty)

  let
    fov = mkFov
      (Height 200)
      (Width 320)
      (Angle 60)

  (e, _) <- RD.elAttr' "canvas" canvasAttrs RD.blank
  eDraw <- RD.button "Go"

  dCx <- fmap CD._canvasInfo_context
    <$> CD.dContext2d (CD.CanvasConfig e mempty)

  let
    stepSquares
      :: Room
      -> Int
      -> (V2 Int -> Int -> Ray -> V2 Int)
      -> Ray
      -> V2 Int
      -> Maybe ((V2 Int,Sqr), V2 Int)
    stepSquares rm sqrS fn ray v@(V2 x y) =
      (\s -> ((v,s), fn v sqrS ray)) <$> T.atPos (R.toRoomCoord sqrS x y) rm

    mkHorizInters r p =
      let
        (T.HorizAX hx, T.HorizAY hy) = R.firstHorizontalIntersection r p sqrSize
      in
        L.unfoldr (stepSquares T.room1 sqrSize R.stepHorizontalIntersection r) (V2 hx hy)

    mkVertInters r p =
      let
        (T.VertAX vx, T.VertAY vy) = R.firstVerticalIntersection r p sqrSize
      in
        L.unfoldr (stepSquares T.room1 sqrSize R.stepVerticalIntersection r) (V2 vx vy)

    drawInters
      :: V2 Int
      -> DrawM ()
    drawInters (V2 x y) = do
      cx <- get
      C.fillRect cx (fromIntegral x) (fromIntegral y) 10 10
      put cx

    renderMap p r =
      runDrawM $ do
        cx <- get

        renderRoom T.room1
        renderPlayer p

        liftJSM $ C.setFillStyle cx ("blue" :: JSString)
        traverse_ (drawInters . fst) (mkHorizInters r p)
        liftJSM $ C.setFillStyle cx ("green" :: JSString)
        traverse_ (drawInters . fst) (mkVertInters r p)

    rayCast p rIth =
      let
        rA = p ^. T.playerFacing
          . to (`T.subtractAngle` halfFOV)
          . to (`T.addAngle` (T.Angle $ rIth * angleBetweenRays))
          . to (Ray . T._unAngle)

        -- where RayBeta is the angle of the ray that is being cast relative to the viewing angle.
        rB = RayBeta $
          (angleBetweenRays * rIth) +
          (angleBetweenRays * (negate . _unAngle $ halfFOV))
      in
        castSingleRay T.room1 sqrSize rA rB p

    buildRays ply = (\i -> (rayCast ply i, i)) <$> [0..319]

    rays' = buildRays player

    rays cx rs = do
      C.beginPath cx
      traverse_ (\(rc,i) -> renderRayCast cx fov sqrSize i rc) rs
      C.closePath cx
      C.stroke cx

    camRender
      :: [(RayCast, Double)]
      -> CanvasRenderingContext2D
      -> Double
      -> JSM ()
    camRender r cx _ = do
      C.clearRect cx 0 0 320 200
      C.save cx
      C.setFillStyle cx ("lightblue" :: JSString)
      C.fillRect cx 0 0 320 200
      C.setFillStyle cx ("darkgrey" :: JSString)
      C.translate cx 160 100
      rays cx r
      C.restore cx

    -- rays = catMaybes . Par.runPar $ Par.parMap (rayCast player) [0..319]

  eLeftTurn <- RD.button "Turn left a bit"
  eRightTurn <- RD.button "Turn right a bit"

  dPlayer <- R.foldDyn ($) player $ R.mergeWith (.)
    [ (T.playerFacing %~ (`T.subtractAngle` (Angle 1))) <$ eLeftTurn
    , (T.playerFacing %~ (`T.addAngle` (Angle 1))) <$ eRightTurn
    ]

  let
    dFstRay = (^. T.playerFacing . to (`T.subtractAngle` halfFOV) . to (Ray . T._unAngle)) <$> dPlayer

    dHI = mkHorizInters <$> dFstRay <*> dPlayer
    dVI = mkVertInters <$> dFstRay <*> dPlayer

  dRays <- R.holdDyn rays' $
    buildRays <$> R.updated dPlayer

  let eMoved = R.leftmost
        [ eDraw
        , eLeftTurn
        , eRightTurn
        ]

  eRendered <- CD.drawWithCx dCx (renderMap <$> dPlayer <*> dFstRay) eMoved
  eRendered' <- CD.drawWithCx dCamCx (camRender <$> dRays) eMoved

  dRendered <- R.holdDyn "Not Rendered" $
    "Rendered!" <$ eRendered

  RD.divClass "DEBUG" $
    RD.display dRendered

  RD.divClass "DEBUG" $
    RD.display dPlayer

  RD.divClass "DEBUG" $
    RD.display dFstRay

  RD.divClass "DEBUG" $
    RD.display dHI

  RD.divClass "DEBUG" $
    RD.display dVI

main :: IO ()
main = run 3911 $ mainWidget app

renderSqr :: V2 Double -> Sqr -> DrawM ()
renderSqr pos Sqr {..} = do
  cx <- get
  C.setFillStyle cx (sqColour _sqType)
  C.fillRect cx (p _x) (p _y) wh wh
  C.setStrokeStyle cx ("black" :: JSString)
  C.strokeRect cx (p _x) (p _y) (realToFrac wh) (realToFrac wh)
  put cx
  where
    p l = pos ^. l . to realToFrac

    sqColour :: SqType -> JSString
    sqColour Wall  = "darkgrey"
    sqColour Floor = "aqua"

    wh = fromIntegral _sqSide

renderPlayer :: T.P -> DrawM ()
renderPlayer (T.P pos face) = do
  cx <- get
  C.save cx
  let
    faceRadians = realToFrac $ (T._unAngle face) * pi / 180
    rCos = cos faceRadians
    rSin = sin faceRadians

  C.transform cx rCos rSin (negate rSin) rCos
    (pos ^. _x . to realToFrac)
    (pos ^. _y . to realToFrac)

  C.setStrokeStyle cx ("red" :: JSString)
  C.strokeRect cx 0 0 10 10

  C.setLineWidth cx 2

  C.beginPath cx
  C.moveTo cx 0 0
  C.lineTo cx 0 256
  C.closePath cx
  C.stroke cx
  C.restore cx
  put cx

renderRoom :: Room -> DrawM ()
renderRoom (Room rm) =
  let
    o = V2 0.0 0.0
    xStep = V2 64.0 0.0
    yStep = V2 0.0  64.0

    rSqrs sq offs =
      renderSqr offs sq $> offs
  in
    --- Hrmmm
    void $ foldlM
      (\o' sqs ->
          yStep + o' <$ foldlM
          (\o'' s ->
              (xStep +) <$> rSqrs s o''
          ) o' sqs
      ) o rm
