{-|
Module: Flaw.Graphics.Program.SL
Description: General helpers for HLSL/GLSL compilers.
License: MIT
-}

{-# LANGUAGE GADTs, OverloadedStrings #-}

module Flaw.Graphics.Program.SL
	( ProgramInfo(..)
	, ShaderInfo(..)
	, programInfo
	) where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.List

import Flaw.Graphics.Program.Internal

-- | Information about node: what temps, attributes, etc. it uses (recursively, i.e. including all subnodes).
type NodeInfo = ([Int], [Attribute], [Uniform], [Sampler])

nodeInfo :: Node a -> NodeInfo
nodeInfo node = case node of
	AttributeNode a -> ([], [a], [], [])
	UniformNode u -> ([], [], [u], [])
	TempNode t -> ([t], [], [], [])
	ConstNode _ _ -> emptyNodeInfo
	IndexNode _ _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	AddNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	SubtractNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	MultiplyNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	DivideNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	RecipNode _ a -> nodeInfo a
	NegateNode _ a -> nodeInfo a
	AbsNode _ a -> nodeInfo a
	SignumNode _ a -> nodeInfo a
	MinNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	MaxNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	ClampNode _ a b c -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) (nodeInfo c)
	LerpNode _ a b c -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) (nodeInfo c)
	EqualNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	LessNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	LessEqualNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	IfNode _ c a b -> mergeNodeInfo (nodeInfo c) $ mergeNodeInfo (nodeInfo a) (nodeInfo b)
	PiNode _ -> emptyNodeInfo
	ExpNode _ a -> nodeInfo a
	SqrtNode _ a -> nodeInfo a
	InvSqrtNode _ a -> nodeInfo a
	LogNode _ a -> nodeInfo a
	PowNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	LogBaseNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	SinNode _ a -> nodeInfo a
	TanNode _ a -> nodeInfo a
	CosNode _ a -> nodeInfo a
	AsinNode _ a -> nodeInfo a
	AtanNode _ a -> nodeInfo a
	AcosNode _ a -> nodeInfo a
	SinhNode _ a -> nodeInfo a
	TanhNode _ a -> nodeInfo a
	CoshNode _ a -> nodeInfo a
	AsinhNode _ a -> nodeInfo a
	AtanhNode _ a -> nodeInfo a
	AcoshNode _ a -> nodeInfo a
	MulNode _ _ _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	DotNode _ _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	CrossNode _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	NormNode _ _ a -> nodeInfo a
	Norm2Node _ _ a -> nodeInfo a
	NormalizeNode _ a -> nodeInfo a
	DdxNode _ a -> nodeInfo a
	DdyNode _ a -> nodeInfo a
	InstanceIdNode -> emptyNodeInfo
	ComponentNode _ _ _ a -> nodeInfo a
	SwizzleNode _ _ _ a -> nodeInfo a
	SampleNode
		{ sampleNodeSamplerNode = SamplerNode s
		, sampleNodeCoordsNode = c
		, sampleNodeOffsetNode = mo
		, sampleNodeLod = ll
		} -> let
		baseInfo = mergeNodeInfo ([], [], [], [s]) (nodeInfo c)
		infoWithOffset = case mo of
			Just o -> mergeNodeInfo baseInfo (nodeInfo o)
			Nothing -> baseInfo
		infoWithLod = case ll of
			SampleNodeAutoLod -> infoWithOffset
			SampleNodeLod l -> mergeNodeInfo infoWithOffset (nodeInfo l)
			SampleNodeBiasLod b -> mergeNodeInfo infoWithOffset (nodeInfo b)
			SampleNodeGradLod gx gy -> mergeNodeInfo infoWithOffset $ mergeNodeInfo (nodeInfo gx) (nodeInfo gy)
		in infoWithLod
	CastNode _ _ a -> nodeInfo a
	Combine2VecNode _ _ _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	Combine3VecNode _ _ _ _ a b c -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) (nodeInfo c)
	Combine4VecNode _ _ _ _ _ a b c d -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) $ mergeNodeInfo (nodeInfo c) (nodeInfo d)
	ScreenToTextureNode _ a -> nodeInfo a
	NormalizeSampledDepthNode a -> nodeInfo a
	FragCoordNode -> ([], [], [], [])

targetNodeInfo :: Target -> NodeInfo
targetNodeInfo target = case target of
	PositionTarget node -> nodeInfo node
	ColorTarget _ node -> nodeInfo node
	DualColorTarget a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	DepthTarget node -> nodeInfo node

mergeNodeInfo :: NodeInfo -> NodeInfo -> NodeInfo
mergeNodeInfo (t1, a1, u1, s1) (t2, a2, u2, s2) = (mergeSorted t1 t2, mergeSorted a1 a2, mergeSorted u1 u2, mergeSorted s1 s2)

emptyNodeInfo :: NodeInfo
emptyNodeInfo = ([], [], [], [])

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted a [] = a
mergeSorted [] a = a
mergeSorted aa@(a:as) bb@(b:bs) = if a < b then a : mergeSorted as bb else b : mergeSorted aa bs

uniqueFromSorted :: Ord a => [a] -> [a]
uniqueFromSorted a = map head $ group a

-- | Information about shader, containing merged lists of all resources and variables used by shader.
data ShaderInfo = ShaderInfo
	{ shaderTemps :: [Temp]
	, shaderAttributes :: [Attribute]
	, shaderUniforms :: [Uniform]
	, shaderSamplers :: [Sampler]
	, shaderTargets :: [Target]
	} deriving Show

shaderInfo :: [Temp] -> [Target] -> V.Vector Temp -> ShaderInfo
shaderInfo sourceTemps targets allTemps = totalInfo where
	-- get information about temps defined in the shader, in order to include everything needed to construct those temps
	tempInfos = map (\Temp { tempNode = node, tempIndex = i } -> mergeNodeInfo ([i], [], [], []) $ nodeInfo node) sourceTemps
	-- get information about target nodes
	targetInfos = map targetNodeInfo targets
	-- merge above things
	(tempIndices, attributes, uniforms, samplers) = foldr mergeNodeInfo (foldr mergeNodeInfo emptyNodeInfo tempInfos) targetInfos
	-- get temps by indices
	temps = map (allTemps V.!) $ uniqueFromSorted tempIndices
	-- combine everything
	totalInfo = ShaderInfo
		{ shaderTemps = temps
		, shaderAttributes = uniqueFromSorted attributes
		, shaderUniforms = uniqueFromSorted uniforms
		, shaderSamplers = uniqueFromSorted samplers
		, shaderTargets = targets
		}

newtype ProgramInfo = ProgramInfo [(Stage, ShaderInfo)] deriving Show

targetStage :: Target -> Stage
targetStage target = case target of
	PositionTarget _ -> VertexStage
	ColorTarget _ _ -> PixelStage
	DualColorTarget _ _ -> PixelStage
	DepthTarget _ -> PixelStage

-- | Generate shader programs in SL.
programInfo :: State -> ProgramInfo
programInfo State
	{ stateTemps = temps
	, stateTargets = targets
	} = info where
	-- create vector with all temps indexed by their indices
	allTemps = V.create $ do
		ts <- VM.new $ length temps
		forM_ temps $ \temp@Temp
			{ tempIndex = i
			} -> VM.write ts i temp
		return ts
	-- group temps by stage
	compareTempsByStage a b = compare (tempStage a) (tempStage b)
	eqTempsByStage a b = tempStage a == tempStage b
	tempsByStage = groupBy eqTempsByStage $ sortBy compareTempsByStage temps
	-- group targets by stage
	compareTargetsByStage a b = compare (targetStage a) (targetStage b)
	eqTargetsByStage a b = targetStage a == targetStage b
	targetsByStage = groupBy eqTargetsByStage $ sortBy compareTargetsByStage targets
	-- calculate shader info for every non-empty stage (i.e. which has either temps or targets)
	getShaderInfo tempGroups@(tempGroup : restTempGroups) targetGroups@(targetGroup : restTargetGroups) = case compare tempGroupStage targetGroupStage of
		LT -> (tempGroupStage, shaderInfo tempGroup [] allTemps) : getShaderInfo restTempGroups targetGroups
		EQ -> (tempGroupStage, shaderInfo tempGroup targetGroup allTemps) : getShaderInfo restTempGroups restTargetGroups
		GT -> (targetGroupStage, shaderInfo [] targetGroup allTemps) : getShaderInfo tempGroups restTargetGroups
		where
			tempGroupStage = tempStage $ head tempGroup
			targetGroupStage = targetStage $ head targetGroup
	getShaderInfo (tempGroup : restTempGroups) [] = (tempStage $ head tempGroup, shaderInfo tempGroup [] allTemps) : getShaderInfo restTempGroups []
	getShaderInfo [] (targetGroup : restTargetGroups) = (targetStage $ head targetGroup, shaderInfo [] targetGroup allTemps) : getShaderInfo [] restTargetGroups
	getShaderInfo [] [] = []
	-- pack results
	info = ProgramInfo $ getShaderInfo tempsByStage targetsByStage
