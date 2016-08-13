{-|
Module: Flaw.Graphics.Vulkan
Description: Vulkan graphics implementation.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Flaw.Graphics.Vulkan
	( VulkanSystem(..)
	, DeviceId(..)
	, DisplayId(..)
	, DisplayModeId(..)
	, initVulkanSystem
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Build
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.Vulkan.FFI

data VulkanSystem = VulkanSystem
	{ vulkanSystemInstance :: {-# UNPACK #-} !VkInstance
	, vulkanSystem_vkEnumeratePhysicalDevices :: !FN_vkEnumeratePhysicalDevices
	, vulkanSystem_vkGetPhysicalDeviceProperties :: !FN_vkGetPhysicalDeviceProperties
	, vulkanSystem_vkGetPhysicalDeviceFeatures :: !FN_vkGetPhysicalDeviceFeatures
	, vulkanSystem_vkGetPhysicalDeviceQueueFamilyProperties :: !FN_vkGetPhysicalDeviceQueueFamilyProperties
	, vulkanSystem_vkCreateDevice :: !FN_vkCreateDevice
	}

initVulkanSystem :: T.Text -> IO (VulkanSystem, IO ())
initVulkanSystem appName = withSpecialBook $ \bk -> do

	-- allocation callbacks (currently null)
	let allocationCallbacksPtr = nullPtr

	-- initialize instance
	inst <- alloca $ \instancePtr -> do
		vulkanCheckResult $ B.useAsCString (T.encodeUtf8 appName) $ \appNamePtr ->
			with VkApplicationInfo
				{ f_VkApplicationInfo_sType = VK_STRUCTURE_TYPE_APPLICATION_INFO
				, f_VkApplicationInfo_pNext = nullPtr
				, f_VkApplicationInfo_pApplicationName = appNamePtr
				, f_VkApplicationInfo_applicationVersion = 0
				, f_VkApplicationInfo_pEngineName = $(embedCStringExp "FLAW")
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
			vkCreateInstance instanceCreateInfoPtr allocationCallbacksPtr instancePtr
		peek instancePtr
	book bk $ return ((), vkDestroyInstance inst allocationCallbacksPtr)

	VulkanSystem inst
		<$> $(vkGetInstanceProc "vkEnumeratePhysicalDevices")
		<*> $(vkGetInstanceProc "vkGetPhysicalDeviceProperties")
		<*> $(vkGetInstanceProc "vkGetPhysicalDeviceFeatures")
		<*> $(vkGetInstanceProc "vkGetPhysicalDeviceQueueFamilyProperties")
		<*> $(vkGetInstanceProc "vkCreateDevice")

instance System VulkanSystem where
	newtype DeviceId VulkanSystem = VulkanDeviceId VkPhysicalDevice
	data DisplayId VulkanSystem = VulkanDisplayId
	data DisplayModeId VulkanSystem = VulkanDisplayModeId
	getInstalledDevices VulkanSystem
		{ vulkanSystemInstance = inst
		, vulkanSystem_vkEnumeratePhysicalDevices = vkEnumeratePhysicalDevices
		, vulkanSystem_vkGetPhysicalDeviceProperties = vkGetPhysicalDeviceProperties
		, vulkanSystem_vkGetPhysicalDeviceQueueFamilyProperties = vkGetPhysicalDeviceQueueFamilyProperties
		} = withSpecialBook $ \_bk -> do
		physicalDevices <- with 0 $ \devicesCountPtr -> do
			vulkanCheckResult $ vkEnumeratePhysicalDevices inst devicesCountPtr nullPtr
			devicesCount <- fromIntegral <$> peek devicesCountPtr
			allocaArray devicesCount $ \devicesPtr -> do
				vulkanCheckResult $ vkEnumeratePhysicalDevices inst devicesCountPtr devicesPtr
				peekArray devicesCount devicesPtr
		forM physicalDevices $ \physicalDevice -> do
			VkPhysicalDeviceProperties
				{ f_VkPhysicalDeviceProperties_deviceName = physicalDeviceNameList
				} <- alloca $ \devicePropertiesPtr -> do
				vkGetPhysicalDeviceProperties physicalDevice devicePropertiesPtr
				peek devicePropertiesPtr
			let physicalDeviceNameLength = length physicalDeviceNameList
			physicalDeviceName <- allocaArray (physicalDeviceNameLength + 1) $ \ptr -> do
				pokeArray0 0 ptr physicalDeviceNameList
				T.decodeUtf8 <$> B.packCString ptr
			return (VulkanDeviceId physicalDevice, DeviceInfo
				{ deviceName = physicalDeviceName
				, deviceDisplays = []
				})
	createDisplayMode _ _ _ _ = undefined

vulkanCheckResult :: IO Word32 -> IO ()
vulkanCheckResult io = do
	r <- io
	unless (r == 0 {- VK_SUCCESS -}) $ throwIO $ DescribeFirstException ("Vulkan error: " ++ show (toEnum (fromIntegral r) :: VkResult))
