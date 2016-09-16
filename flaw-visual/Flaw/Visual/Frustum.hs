{-|
Module: Flaw.Visual.Frustum
Description: Frustum handling.
License: MIT
-}

module Flaw.Visual.Frustum
	( Frustum(..)
	, identityFrustum
	, perspectiveFrustum
	, lookAtFrustum
	, FrustumNode(..)
	, uniformFrustum
	, renderUniformFrustum
	) where

import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Math
import Flaw.Math.Geometry

-- | Frustum data structure.
-- Near and far plane distances must be > 0,
-- but linear depths corresponding to homogeneous 0 and (-1) are
-- (-frustumNear) and (-frustumFar) respectively
-- (because spaces are right-handled).
data Frustum = Frustum
	{
	-- | Eye position.
	  frustumEye :: !Float3
	-- | View matrix.
	, frustumView :: !Float4x4
	-- | Inverse view matrix.
	, frustumInvView :: Float4x4
	-- | Near plane distance.
	, frustumNear :: {-# UNPACK #-} !Float
	-- | Far plane distance.
	, frustumFar :: {-# UNPACK #-} !Float
	-- | Projection matrix (calculated via 'projectionPerspectiveFov').
	, frustumProj :: !Float4x4
	-- | Inverse projection matrix.
	, frustumInvProj :: Float4x4
	-- | View-projection matrix.
	, frustumViewProj :: Float4x4
	}

-- | Identity frustum.
identityFrustum :: Frustum
identityFrustum = Frustum
	{ frustumEye = 0
	, frustumView = affineIdentity
	, frustumInvView = affineIdentity
	, frustumNear = 0
	, frustumFar = 0
	, frustumProj = affineIdentity
	, frustumInvProj = affineIdentity
	, frustumViewProj = affineIdentity
	}

-- | Set perspective projection for frustum.
perspectiveFrustum :: Float -> Float -> Float -> Float -> Frustum -> Frustum
perspectiveFrustum fovY aspect near far frustum@Frustum
	{ frustumView = view
	} = frustum
	{ frustumNear = near
	, frustumFar = far
	, frustumProj = proj
	, frustumInvProj = matInverse proj
	, frustumViewProj = proj `mul` view
	} where
	proj = projectionPerspectiveFov fovY aspect near far

-- | Set look-at view matrix for frustum.
lookAtFrustum :: Float3 -> Float3 -> Float3 -> Frustum -> Frustum
lookAtFrustum eye target up frustum@Frustum
	{ frustumProj = proj
	} = frustum
	{ frustumEye = eye
	, frustumView = view
	, frustumInvView = matInverse view
	, frustumViewProj = proj `mul` view
	} where
	view = affineLookAt eye target up

data FrustumNode = FrustumNode
	{ frustumNodeEye :: !(Node Float3)
	, frustumNodeView :: !(Node Float4x4)
	, frustumNodeInvView :: !(Node Float4x4)
	, frustumNodeNear :: !(Node Float)
	, frustumNodeFar :: !(Node Float)
	, frustumNodeProj :: !(Node Float4x4)
	, frustumNodeInvProj :: !(Node Float4x4)
	, frustumNodeViewProj :: !(Node Float4x4)
	}

uniformFrustum :: UniformBufferSlot -> IO FrustumNode
uniformFrustum ubs = FrustumNode
	<$> uniform ubs
	<*> uniform ubs
	<*> uniform ubs
	<*> uniform ubs
	<*> uniform ubs
	<*> uniform ubs
	<*> uniform ubs
	<*> uniform ubs

renderUniformFrustum :: UniformStorage d -> FrustumNode -> Frustum -> Render c ()
renderUniformFrustum us FrustumNode
	{ frustumNodeEye = eyeNode
	, frustumNodeView = viewNode
	, frustumNodeInvView = invViewNode
	, frustumNodeNear = nearNode
	, frustumNodeFar = farNode
	, frustumNodeProj = projNode
	, frustumNodeInvProj = invProjNode
	, frustumNodeViewProj = viewProjNode
	} Frustum
	{ frustumEye = eye
	, frustumView = view
	, frustumInvView = invView
	, frustumNear = near
	, frustumFar = far
	, frustumProj = proj
	, frustumInvProj = invProj
	, frustumViewProj = viewProj
	} = do
	renderUniform us eyeNode eye
	renderUniform us viewNode view
	renderUniform us invViewNode invView
	renderUniform us nearNode near
	renderUniform us farNode far
	renderUniform us projNode proj
	renderUniform us invProjNode invProj
	renderUniform us viewProjNode viewProj
