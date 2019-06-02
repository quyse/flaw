{-|
Module: Flaw.Visual.Pipeline
Description: Render pipeline stuff.
License: MIT
-}

module Flaw.Visual.Pipeline
  ( newPipelineWrapper
  ) where

import Control.Monad.IO.Class
import Data.IORef

import Flaw.Book
import Flaw.Graphics
import Flaw.Math

{-# INLINEABLE newPipelineWrapper #-}
newPipelineWrapper :: Context c d => (Int -> Int -> IO (p, IO ())) -> IO (Render c p, IO ())
newPipelineWrapper newPipeline = withSpecialBook $ \sbk -> do
  bk <- book sbk newDynamicBook
  initialPipeline <- book bk $ newPipeline 4 4
  ref <- newIORef (initialPipeline, 4, 4)
  return $ do
    (currentPipeline, width, height) <- liftIO $ readIORef ref

    -- re-create pipeline if viewport size has changed
    Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
    let viewportWidth = viewportRight - viewportLeft
    let viewportHeight = viewportBottom - viewportTop
    if width == viewportWidth && height == viewportHeight then return currentPipeline else liftIO $ do
      freeBook bk
      createdPipeline <- book bk $ newPipeline viewportWidth viewportHeight
      writeIORef ref (createdPipeline, viewportWidth, viewportHeight)
      return createdPipeline
