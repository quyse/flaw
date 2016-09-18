{-|
Module: Flaw.Visual.Frustum
Description: Frustum handling.
License: MIT
-}

module Flaw.Visual.Frustum
	( Frustum(..)
	, identityFrustum
	, orthoFrustum
	, perspectiveFrustum
	, lookAtFrustum
	, FrustumNode(..)
	, uniformFrustum
	, renderUniformFrustum
	, frustumDepthHomogeneousToLinear
	, frustumProjCoordToLinearDepth
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
	-- | Projection params.
	-- Chosen to work universally for both ortho and perspective projections,
	-- so we can use universal formulae in 'frustumDepthHomogeneousToLinear'
	-- and 'frustumProjCoordToLinearDepth'.
	, frustumProjParams :: !Float4
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
	, frustumProjParams = 0
	, frustumViewProj = affineIdentity
	}

-- | Set orthographic projection for frustum.
orthoFrustum :: Float -> Float -> Float -> Float -> Frustum -> Frustum
orthoFrustum width height near far frustum@Frustum
	{ frustumView = view
	} = frustum
	{ frustumNear = near
	, frustumFar = far
	, frustumProj = proj
	, frustumInvProj = matInverse proj
	, frustumProjParams = Float4 (-far) 0 (far - near) (-far)
	, frustumViewProj = proj `mul` view
	} where
	proj = projectionOrtho width height (-far) (-near)

-- | Set perspective projection for frustum.
perspectiveFrustum :: Float -> Float -> Float -> Float -> Frustum -> Frustum
perspectiveFrustum fovY aspect near far frustum@Frustum
	{ frustumView = view
	} = frustum
	{ frustumNear = near
	, frustumFar = far
	, frustumProj = proj
	, frustumInvProj = matInverse proj
	, frustumProjParams = Float4 (-far) (far / near - 1) 0 (-1)
	, frustumViewProj = proj `mul` view
	} where
	-- use inverse projection for better precision distribution
	-- (far plane maps to zero, near plane maps to 1)
	proj = projectionPerspectiveFov fovY aspect (-far) (-near)

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
	-- | (z0, z1, z0 / z1 - 1)
	, frustumNodeProjParams :: !(Node Float4)
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

renderUniformFrustum :: UniformStorage d -> FrustumNode -> Frustum -> Render c ()
renderUniformFrustum us FrustumNode
	{ frustumNodeEye = eyeNode
	, frustumNodeView = viewNode
	, frustumNodeInvView = invViewNode
	, frustumNodeProjParams = projParamsNode
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
	, frustumProjParams = projParams
	, frustumViewProj = viewProj
	} = do
	renderUniform us eyeNode eye
	renderUniform us viewNode view
	renderUniform us invViewNode invView
	renderUniform us projParamsNode projParams
	renderUniform us projNode proj
	renderUniform us invProjNode invProj
	renderUniform us viewProjNode viewProj

-- | Get linear depth from homogeneous depth.
frustumDepthHomogeneousToLinear :: FrustumNode -> Node Float -> Node Float
frustumDepthHomogeneousToLinear FrustumNode
	{ frustumNodeProjParams = p
	} z = x_ p / (1 + z * y_ p) + z_ p * z

-- | Get linear depth from projection-space coord.
frustumProjCoordToLinearDepth :: FrustumNode -> Node Float4 -> Node Float
frustumProjCoordToLinearDepth FrustumNode
	{ frustumNodeProjParams = p
	} c = dot (zw__ c) (zw__ p)
