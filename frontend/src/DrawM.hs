{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
module DrawM where

import           JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import           JSDOM.Types                    (JSM, MonadJSM, liftJSM)

import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.State            (MonadState, StateT, evalStateT,
                                                 execStateT, get, lift,
                                                 runStateT)

newtype DrawM a = DrawM
  { unDrawM :: StateT CanvasRenderingContext2D JSM a
  }
  deriving ( Functor
           , Applicative
           , Monad
           -- , MonadIO
           -- , MonadJSM
           , MonadState CanvasRenderingContext2D
           )

#ifdef ghcjs_HOST_OS
deriving instance MonadJSM DrawM
#endif

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM DrawM
deriving instance MonadIO DrawM
#endif

runDrawM
  :: DrawM a
  -> CanvasRenderingContext2D
  -> Double
  -> JSM a
runDrawM dM cr _ =
  evalStateT (unDrawM dM) cr
