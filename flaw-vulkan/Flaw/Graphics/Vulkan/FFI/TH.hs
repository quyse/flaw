{-|
Module: Flaw.Graphics.Vulkan.FFI.TH
Description: Vulkan FFI internal TH routines.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.Vulkan.FFI.TH
	( vkDefineHandle
	, vkGetInstanceProc
	) where

import Foreign.Ptr
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import Flaw.Build

-- | Define type synonym for a pointer to anonymous type.
vkDefineHandle :: String -> Q [Dec]
vkDefineHandle n = sequence
	[ dataD (sequence []) (mkName (n ++ "_T")) [] Nothing [] (sequence [])
	, tySynD (mkName n) [] [t| Ptr $(conT (mkName (n ++ "_T"))) |]
	]

-- | Define top-level foreign wrapper and return instance function of a given name.
-- Function type must be prefixed with exactly "FN_".
-- Also "inst" variable must be in scope.
vkGetInstanceProc :: String -> ExpQ
vkGetInstanceProc n = do
	wrapperName <- newName $ "dyn_" ++ n
	let typeName = mkName $ "FN_" ++ n
	TH.addTopDecls =<< sequence
		[ forImpD cCall safe "dynamic" wrapperName [t| FunPtr $(conT typeName) -> $(conT typeName) |]
		]
	[| $(varE wrapperName) . castFunPtr <$> $(varE $ mkName "vkGetInstanceProcAddr") $(varE $ mkName "inst") $(embedCStringExp n) |]
