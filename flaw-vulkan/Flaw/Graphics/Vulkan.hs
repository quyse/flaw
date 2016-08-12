{-|
Module: Flaw.Graphics.Vulkan
Description: Vulkan graphics implementation.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.Graphics.Vulkan
	( VulkanSystem(..)
	, initVulkanSystem
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception
import Flaw.Graphics.Vulkan.FFI

data VulkanSystem = VulkanSystem
	{ vulkanSystemInstance :: {-# UNPACK #-} !VkInstance
	}

initVulkanSystem :: T.Text -> IO VulkanSystem
initVulkanSystem appName = VulkanSystem
	<$> (alloca $ \instancePtr -> do
		vulkanCheckResult $ B.useAsCString (T.encodeUtf8 "FLAW") $ \engineNamePtr -> B.useAsCString (T.encodeUtf8 appName) $ \appNamePtr ->
			with VkApplicationInfo
				{ f_VkApplicationInfo_sType = VK_STRUCTURE_TYPE_APPLICATION_INFO
				, f_VkApplicationInfo_pNext = nullPtr
				, f_VkApplicationInfo_pApplicationName = appNamePtr
				, f_VkApplicationInfo_applicationVersion = 0
				, f_VkApplicationInfo_pEngineName = engineNamePtr
				, f_VkApplicationInfo_engineVersion = 0
				, f_VkApplicationInfo_apiVersion = VK_API_VERSION
				} $ \applicationInfoPtr ->
			with VkInstanceCreateInfo
				{ f_VkInstanceCreateInfo_sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
				, f_VkInstanceCreateInfo_pNext = nullPtr
				, f_VkInstanceCreateInfo_flags = 0
				, f_VkInstanceCreateInfo_pApplicationInfo = applicationInfoPtr
				, f_VkInstanceCreateInfo_enabledLayerCount = 0
				, f_VkInstanceCreateInfo_ppEnabledLayerNames = nullPtr
				, f_VkInstanceCreateInfo_enabledExtensionCount = 0
				, f_VkInstanceCreateInfo_ppEnabledExtensionNames = nullPtr
				} $ \instanceCreateInfoPtr ->
			vkCreateInstance instanceCreateInfoPtr nullPtr instancePtr
		peek instancePtr
		)

vulkanCheckResult :: IO Word32 -> IO ()
vulkanCheckResult io = do
	r <- io
	unless (r == (fromIntegral $ fromEnum VK_SUCCESS)) $ throwIO $ DescribeFirstException ("Vulkan error: " ++ show r)
