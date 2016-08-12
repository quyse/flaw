{-|
Module: Flaw.Graphics.Vulkan.FFI.TH
Description: Vulkan FFI internal TH routines.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.Vulkan.FFI.TH
	( vkDefineHandle
	) where

import Foreign.Ptr
import Language.Haskell.TH

vkDefineHandle :: String -> Q [Dec]
vkDefineHandle n = sequence
	[ dataD (sequence []) (mkName (n ++ "_T")) [] [] []
	, tySynD (mkName n) [] [t| Ptr $(conT (mkName (n ++ "_T"))) |]
	]
