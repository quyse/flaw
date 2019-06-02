{-|
Module: Flaw.Physics.Util
Description: Physics utilities.
License: MIT
-}

module Flaw.Physics.Util
  ( createFrustumGhost
  ) where

import qualified Data.Vector as V

import Flaw.Book
import Flaw.Math
import Flaw.Math.Transform
import Flaw.Physics

createFrustumGhost :: World w => w -> Float4x4 -> FloatQO -> IO (Ghost w, IO ())
createFrustumGhost world invProj transform = withSpecialBook $ \bk -> do
  shape <- book bk $ createConvexHullShape world $ V.fromList
    [ xyz__ (invProj `mul` (p :: Float4)) | p <-
      [ Vec4 (-1) (-1) 0 1
      , Vec4 (-1) (-1) 1 1
      , Vec4 (-1)   1  0 1
      , Vec4 (-1)   1  1 1
      , Vec4   1  (-1) 0 1
      , Vec4   1  (-1) 1 1
      , Vec4   1    1  0 1
      , Vec4   1    1  1 1
      ]
    ]
  book bk $ createGhost world shape transform
