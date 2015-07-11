{-|
Module: Flaw.Graphics.Program.SL
Description: General helpers for HLSL/GLSL compilers.
License: MIT
-}

{-# LANGUAGE GADTs, OverloadedStrings #-}

module Flaw.Graphics.Program.SL
	( ProgramInfo(..)
	, ShaderInfo
	, programInfo
	) where

import Data.Array
import Data.List

import Flaw.Graphics.Program.Internal

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
	PiNode _ -> emptyNodeInfo
	ExpNode _ a -> nodeInfo a
	SqrtNode _ a -> nodeInfo a
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
	NormNode _ _ a -> nodeInfo a
	Norm2Node _ _ a -> nodeInfo a
	NormalizeNode _ a -> nodeInfo a
	InstanceIdNode -> emptyNodeInfo
	ComponentNode _ _ _ a -> nodeInfo a
	SwizzleNode _ _ _ a -> nodeInfo a
	SampleNode (SamplerNode s) c -> mergeNodeInfo ([], [], [], [s]) (nodeInfo c)
	CastNode _ _ a -> nodeInfo a
	Combine2VecNode _ _ _ a b -> mergeNodeInfo (nodeInfo a) (nodeInfo b)
	Combine3VecNode _ _ _ _ a b c -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) (nodeInfo c)
	Combine4VecNode _ _ _ _ _ a b c d -> mergeNodeInfo (nodeInfo a) $ mergeNodeInfo (nodeInfo b) $ mergeNodeInfo (nodeInfo c) (nodeInfo d)

targetNodeInfo :: Target -> NodeInfo
targetNodeInfo target = case target of
	PositionTarget node -> nodeInfo node
	ColorTarget _ node -> nodeInfo node
	DepthTarget node -> nodeInfo node

mergeNodeInfo :: NodeInfo -> NodeInfo -> NodeInfo
mergeNodeInfo (t1, a1, u1, s1) (t2, a2, u2, s2) = (mergeSorted t1 t2, mergeSorted a1 a2, mergeSorted u1 u2, mergeSorted s1 s2)

emptyNodeInfo :: NodeInfo
emptyNodeInfo = ([], [], [], [])

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted a [] = a
mergeSorted [] a = a
mergeSorted aa@(a:as) bb@(b:bs) = if a < b then a : mergeSorted as bb else b : mergeSorted aa bs

unique :: Ord a => [a] -> [a]
unique a = map head $ group a

type ShaderInfo = ([Temp], [Attribute], [Uniform], [Sampler], [Target])

shaderInfo :: [Temp] -> [Target] -> Array Int Temp -> ShaderInfo
shaderInfo sourceTemps targets allTemps = totalInfo where
	tempInfos = map (\Temp { tempNode = node, tempIndex = i } -> mergeNodeInfo ([i], [], [], []) $ nodeInfo node) sourceTemps
	targetInfos = map targetNodeInfo targets
	(tempIndices, attributes, uniforms, samplers) = foldr mergeNodeInfo (foldr mergeNodeInfo emptyNodeInfo tempInfos) targetInfos
	temps = map (allTemps !) $ unique tempIndices
	totalInfo = (temps, unique attributes, unique uniforms, unique samplers, targets)

data ProgramInfo
	= VertexPixelProgramInfo ShaderInfo ShaderInfo
	deriving Show

targetStage :: Target -> Stage
targetStage target = case target of
	PositionTarget _ -> VertexStage
	ColorTarget _ _ -> PixelStage
	DepthTarget _ -> PixelStage

-- | Generate shader programs in SL.
programInfo :: State -> ProgramInfo
programInfo State
	{ stateTemps = temps
	, stateTargets = targets
	} = info where
	allTemps = array (0, length temps - 1) $ map (\temp@Temp { tempIndex = i } -> (i, temp)) temps
	vertexTemps = filter (\temp -> tempStage temp == VertexStage) temps
	pixelTemps = filter (\temp -> tempStage temp == PixelStage) temps
	vertexTargets = filter (\target -> targetStage target == VertexStage) targets
	pixelTargets = filter (\target -> targetStage target == PixelStage) targets
	vsInfo = shaderInfo vertexTemps vertexTargets allTemps
	psInfo = shaderInfo pixelTemps pixelTargets allTemps
	info = VertexPixelProgramInfo vsInfo psInfo
