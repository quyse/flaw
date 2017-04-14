{-|
Module: Flaw.Graphics.Vulkan.FFI.TH
Description: Vulkan FFI internal TH routines.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.Vulkan.FFI.TH
	( vkDefineHandle
	, vkGetInstanceProc
	, vkGetDeviceProc
	) where

import Foreign.Ptr
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import Flaw.Build
import Flaw.Exception

-- | Define type synonym for a pointer to anonymous type.
vkDefineHandle :: String -> Q [Dec]
vkDefineHandle n = sequence
	[ dataD (sequence []) (mkName (n ++ "_T")) [] Nothing [] (sequence [])
	, tySynD (mkName n) [] [t| Ptr $(conT (mkName (n ++ "_T"))) |]
	]

-- | Define top-level foreign wrapper and return function of a given name.
-- Function type must be prefixed with exactly "FN_".
vkGetProc
	:: ExpQ -- ^ expression for getting function by name
	-> String -- ^ function name
	-> ExpQ
vkGetProc getAddrExp n = do
	wrapperName <- newName $ "dyn_" ++ n
	let typeName = mkName $ "FN_" ++ n
	TH.addTopDecls =<< sequence
		[ forImpD cCall safe "dynamic" wrapperName [t| FunPtr $(conT typeName) -> $(conT typeName) |]
		]
	[| do
		addr <- $getAddrExp $(embedCStringExp n)
		if addr == nullFunPtr then throwIO $ DescribeFirstException $(litE $ stringL $ "failed to get proc " ++ n)
		else return $ $(varE wrapperName) $ castFunPtr addr
		|]

-- | Get instance proc.
-- Special function for system initialization.
-- Variable name "inst" is hardcoded!
vkGetInstanceProc :: String -> ExpQ
vkGetInstanceProc = vkGetProc [| vkGetInstanceProcAddr $(varE $ mkName "inst") |]

-- | Get device proc.
-- Special function for device initialization.
-- Variable names "vkGetDeviceProcAddr" and "device" are hardcoded!
vkGetDeviceProc :: String -> ExpQ
vkGetDeviceProc = vkGetProc [| $(varE $ mkName "vkGetDeviceProcAddr") $(varE $ mkName "device") |]
