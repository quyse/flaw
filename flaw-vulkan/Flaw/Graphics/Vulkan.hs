{-|
Module: Flaw.Graphics.Vulkan
Description: Vulkan graphics implementation.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.Graphics.Vulkan
	( VulkanSystem(..)
	, DeviceId(..)
	, DisplayId(..)
	, DisplayModeId(..)
	, initVulkanSystem
	, VulkanDevice(..)
	, newVulkanDevice
	) where

import Control.Exception
import Control.Monad
import Data.Bits
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
	, vulkanSystem_vkGetDeviceProcAddr :: !FN_vkGetDeviceProcAddr
	}

initVulkanSystem :: T.Text -> IO (VulkanSystem, IO ())
initVulkanSystem appName = describeException "failed to init Vulkan system" $ withSpecialBook $ \bk -> do

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
		<*> $(vkGetInstanceProc "vkGetDeviceProcAddr")

instance System VulkanSystem where
	newtype DeviceId VulkanSystem = VulkanDeviceId VkPhysicalDevice
	data DisplayId VulkanSystem = VulkanDisplayId
	data DisplayModeId VulkanSystem = VulkanDisplayModeId
	getInstalledDevices VulkanSystem
		{ vulkanSystemInstance = inst
		, vulkanSystem_vkEnumeratePhysicalDevices = vkEnumeratePhysicalDevices
		, vulkanSystem_vkGetPhysicalDeviceProperties = vkGetPhysicalDeviceProperties
		} = describeException "failed to get Vulkan installed devices" $ withSpecialBook $ \_bk -> do
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

data VulkanDevice = VulkanDevice
	{ vulkanDevice_device :: {-# UNPACK #-} !VkDevice
	, vulkanDevice_graphicsQueue :: {-# UNPACK #-} !VkQueue
	, vulkanDevice_vkDestroyDevice :: !FN_vkDestroyDevice
	, vulkanDevice_vkGetDeviceQueue :: !FN_vkGetDeviceQueue
	, vulkanDevice_vkCreateImage :: !FN_vkCreateImage
	, vulkanDevice_vkCreateImageView :: !FN_vkCreateImageView
	, vulkanDevice_vkCreateRenderPass :: !FN_vkCreateRenderPass
	, vulkanDevice_vkCreateFramebuffer :: !FN_vkCreateFramebuffer
	}

newVulkanDevice :: VulkanSystem -> DeviceId VulkanSystem -> IO (VulkanDevice, IO ())
newVulkanDevice VulkanSystem
	{ vulkanSystem_vkGetPhysicalDeviceQueueFamilyProperties = vkGetPhysicalDeviceQueueFamilyProperties
	, vulkanSystem_vkCreateDevice = vkCreateDevice
	, vulkanSystem_vkGetDeviceProcAddr = vkGetDeviceProcAddr
	} (VulkanDeviceId physicalDevice) = describeException "failed to create Vulkan device" $ withSpecialBook $ \bk -> do

	-- get info about device queues
	queueFamilies <- with 0 $ \queueFamiliesCountPtr -> do
		vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamiliesCountPtr nullPtr
		queueFamiliesCount <- fromIntegral <$> peek queueFamiliesCountPtr
		allocaArray queueFamiliesCount $ \queueFamiliesPtr -> do
			vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamiliesCountPtr queueFamiliesPtr
			peekArray queueFamiliesCount queueFamiliesPtr

	-- find queue family supporting graphics
	graphicsQueueIndex <- case filter ((> 0) . (.&. fromEnum VK_QUEUE_GRAPHICS_BIT) . fromEnum . f_VkQueueFamilyProperties_queueFlags . fst) $ zip queueFamilies [0..] of
		(VkQueueFamilyProperties {}, i) : _ -> return i
		[] -> throwIO $ DescribeFirstException "no queue families supporting graphics"

	-- create device
	device <- with 1 $ \queuePrioritiesPtr -> do
		let deviceQueueCreateInfos =
			[ VkDeviceQueueCreateInfo
				{ f_VkDeviceQueueCreateInfo_sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
				, f_VkDeviceQueueCreateInfo_pNext = nullPtr
				, f_VkDeviceQueueCreateInfo_flags = 0
				, f_VkDeviceQueueCreateInfo_queueFamilyIndex = graphicsQueueIndex
				, f_VkDeviceQueueCreateInfo_queueCount = 1
				, f_VkDeviceQueueCreateInfo_pQueuePriorities = queuePrioritiesPtr
				}
			]
		withArray deviceQueueCreateInfos $ \deviceQueueCreateInfoPtr -> with VkDeviceCreateInfo
			{ f_VkDeviceCreateInfo_sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
			, f_VkDeviceCreateInfo_pNext = nullPtr
			, f_VkDeviceCreateInfo_flags = 0
			, f_VkDeviceCreateInfo_queueCreateInfoCount = fromIntegral $ length deviceQueueCreateInfos
			, f_VkDeviceCreateInfo_pQueueCreateInfos = deviceQueueCreateInfoPtr
			, f_VkDeviceCreateInfo_enabledLayerCount = 0
			, f_VkDeviceCreateInfo_ppEnabledLayerNames = nullPtr
			, f_VkDeviceCreateInfo_enabledExtensionCount = 0
			, f_VkDeviceCreateInfo_ppEnabledExtensionNames = nullPtr
			, f_VkDeviceCreateInfo_pEnabledFeatures = nullPtr
			} $ \deviceCreateInfoPtr ->
			alloca $ \devicePtr -> do
				vulkanCheckResult $ vkCreateDevice physicalDevice deviceCreateInfoPtr nullPtr devicePtr
				peek devicePtr

	-- get functions
	vkDestroyDevice <- $(vkGetDeviceProc "vkDestroyDevice")
	vkGetDeviceQueue <- $(vkGetDeviceProc "vkGetDeviceQueue")
	vkCreateImage <- $(vkGetDeviceProc "vkCreateImage")
	vkCreateImageView <- $(vkGetDeviceProc "vkCreateImageView")
	vkCreateRenderPass <- $(vkGetDeviceProc "vkCreateRenderPass")
	vkCreateFramebuffer <- $(vkGetDeviceProc "vkCreateFramebuffer")

	-- book finalizer
	book bk $ return ((), vkDestroyDevice device nullPtr)

	-- get queues
	graphicsQueue <- alloca $ \queuePtr -> do
		vkGetDeviceQueue device graphicsQueueIndex 0 queuePtr
		peek queuePtr

	return VulkanDevice
		{ vulkanDevice_device = device
		, vulkanDevice_graphicsQueue = graphicsQueue
		, vulkanDevice_vkDestroyDevice = vkDestroyDevice
		, vulkanDevice_vkGetDeviceQueue = vkGetDeviceQueue
		, vulkanDevice_vkCreateImage = vkCreateImage
		, vulkanDevice_vkCreateImageView = vkCreateImageView
		, vulkanDevice_vkCreateRenderPass = vkCreateRenderPass
		, vulkanDevice_vkCreateFramebuffer = vkCreateFramebuffer
		}

vulkanCheckResult :: IO Word32 -> IO ()
vulkanCheckResult io = do
	r <- io
	unless (r == 0 {- VK_SUCCESS -}) $ throwIO $ DescribeFirstException ("Vulkan error: " ++ show (toEnum (fromIntegral r) :: VkResult))
