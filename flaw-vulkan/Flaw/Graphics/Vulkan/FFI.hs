{-|
Module: Flaw.Graphics.Vulkan.FFI
Description: Vulkan FFI.
License: MIT
-}

{-# LANGUAGE CPP, PatternSynonyms, TemplateHaskell #-}

module Flaw.Graphics.Vulkan.FFI
	(
	-- * version
	  pattern VK_API_VERSION
	-- * general
	, VkResult(..)
	, VkStructureType(..)
	, VkFlags
	, VkBool32
	, VkExtent3D(..)
	-- * allocation
	, VkSystemAllocationScope(..)
	, VkInternalAllocationType(..)
	, FN_vkAllocationFunction
	, FN_vkReallocationFunction
	, FN_vkFreeFunction
	, FN_vkInternalAllocationNotification
	, FN_vkInternalFreeNotification
	, VkAllocationCallbacks(..)
	-- * instance
	, VkInstance
	, VkApplicationInfo(..)
	, VkInstanceCreateFlags
	, VkInstanceCreateInfo(..)
	, vkCreateInstance
	, vkDestroyInstance
	, vkGetInstanceProcAddr
	, vkGetInstanceProc
	-- * physical device
	, VkPhysicalDevice
	, FN_vkEnumeratePhysicalDevices
	, VkPhysicalDeviceType(..)
	, VkDeviceSize
	, VkSampleCountFlags
	, VkPhysicalDeviceLimits(..)
	, VkPhysicalDeviceSparseProperties(..)
	, VkPhysicalDeviceProperties(..)
	, FN_vkGetPhysicalDeviceProperties
	, VkPhysicalDeviceFeatures(..)
	, FN_vkGetPhysicalDeviceFeatures
	, VkQueueFlags
	, VkQueueFlagBits(..)
	, VkQueueFamilyProperties(..)
	, FN_vkGetPhysicalDeviceQueueFamilyProperties
	-- * device
	, VkDevice
	, VkDeviceCreateFlags
	, VkDeviceQueueCreateFlags
	, VkDeviceQueueCreateInfo(..)
	, VkDeviceCreateInfo(..)
	, FN_vkCreateDevice
	, FN_vkDestroyDevice
	-- * queue
	, VkQueue
	, FN_vkGetDeviceQueue
	) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

import Flaw.FFI
import Flaw.Graphics.Vulkan.FFI.TH

-- Vulkan API convention for foreign definitions
#if defined(mingw32_HOST_OS)
#define VKAPI_CALL stdcall
#else
#define VKAPI_CALL ccall
#endif


-- version
pattern VK_API_VERSION :: Word32
pattern VK_API_VERSION = 0x400018 -- 1.0.24


-- general

-- | VkResult
genEnum [t|Word32|] "VkResult"
	[ ("VK_SUCCESS", 0)
	, ("VK_NOT_READY", 1)
	, ("VK_TIMEOUT", 2)
	, ("VK_EVENT_SET", 3)
	, ("VK_EVENT_RESET", 4)
	, ("VK_INCOMPLETE", 5)
	, ("VK_ERROR_OUT_OF_HOST_MEMORY", 0x100000000 - 1)
	, ("VK_ERROR_OUT_OF_DEVICE_MEMORY", 0x100000000 - 2)
	, ("VK_ERROR_INITIALIZATION_FAILED", 0x100000000 - 3)
	, ("VK_ERROR_DEVICE_LOST", 0x100000000 - 4)
	, ("VK_ERROR_MEMORY_MAP_FAILED", 0x100000000 - 5)
	, ("VK_ERROR_LAYER_NOT_PRESENT", 0x100000000 - 6)
	, ("VK_ERROR_EXTENSION_NOT_PRESENT", 0x100000000 - 7)
	, ("VK_ERROR_FEATURE_NOT_PRESENT", 0x100000000 - 8)
	, ("VK_ERROR_INCOMPATIBLE_DRIVER", 0x100000000 - 9)
	, ("VK_ERROR_TOO_MANY_OBJECTS", 0x100000000 - 10)
	, ("VK_ERROR_FORMAT_NOT_SUPPORTED", 0x100000000 - 11)
	, ("VK_ERROR_FRAGMENTED_POOL", 0x100000000 - 12)
	, ("VK_ERROR_SURFACE_LOST_KHR", 0x100000000 - 1000000000)
	, ("VK_ERROR_NATIVE_WINDOW_IN_USE_KHR", 0x100000000 - 1000000001)
	, ("VK_SUBOPTIMAL_KHR", 1000001003)
	, ("VK_ERROR_OUT_OF_DATE_KHR", 0x100000000 - 1000001004)
	, ("VK_ERROR_INCOMPATIBLE_DISPLAY_KHR", 0x100000000 - 1000003001)
	, ("VK_ERROR_VALIDATION_FAILED_EXT", 0x100000000 - 1000011001)
	, ("VK_ERROR_INVALID_SHADER_NV", 0x100000000 - 1000012000)
	]


-- structures

-- | VkStructureType
genEnum [t|Word32|] "VkStructureType"
	[ ("VK_STRUCTURE_TYPE_APPLICATION_INFO", 0)
	, ("VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO", 1)
	, ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO", 2)
	, ("VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO", 3)
	, ("VK_STRUCTURE_TYPE_SUBMIT_INFO", 4)
	, ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO", 5)
	, ("VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE", 6)
	, ("VK_STRUCTURE_TYPE_BIND_SPARSE_INFO", 7)
	, ("VK_STRUCTURE_TYPE_FENCE_CREATE_INFO", 8)
	, ("VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO", 9)
	, ("VK_STRUCTURE_TYPE_EVENT_CREATE_INFO", 10)
	, ("VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO", 11)
	, ("VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO", 12)
	, ("VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO", 13)
	, ("VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO", 14)
	, ("VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO", 15)
	, ("VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO", 16)
	, ("VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO", 17)
	, ("VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO", 18)
	, ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO", 19)
	, ("VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO", 20)
	, ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO", 21)
	, ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO", 22)
	, ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO", 23)
	, ("VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO", 24)
	, ("VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO", 25)
	, ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO", 26)
	, ("VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO", 27)
	, ("VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO", 28)
	, ("VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO", 29)
	, ("VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO", 30)
	, ("VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO", 31)
	, ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO", 32)
	, ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO", 33)
	, ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO", 34)
	, ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET", 35)
	, ("VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET", 36)
	, ("VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO", 37)
	, ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO", 38)
	, ("VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO", 39)
	, ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO", 40)
	, ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO", 41)
	, ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO", 42)
	, ("VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO", 43)
	, ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER", 44)
	, ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER", 45)
	, ("VK_STRUCTURE_TYPE_MEMORY_BARRIER", 46)
	, ("VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO", 47)
	, ("VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO", 48)
	, ("VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR", 1000001000)
	, ("VK_STRUCTURE_TYPE_PRESENT_INFO_KHR", 1000001001)
	, ("VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR", 1000002000)
	, ("VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR", 1000002001)
	, ("VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR", 1000003000)
	, ("VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR", 1000004000)
	, ("VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR", 1000005000)
	, ("VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR", 1000006000)
	, ("VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR", 1000007000)
	, ("VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR", 1000008000)
	, ("VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR", 1000009000)
	, ("VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT", 1000011000)
	, ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD", 1000018000)
	, ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT", 1000022000)
	, ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT", 1000022001)
	, ("VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT", 1000022002)
	, ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV", 1000026000)
	, ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV", 1000026001)
	, ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV", 1000026002)
	]

type VkFlags = Word32
type VkBool32 = Word32

genStruct "VkExtent3D"
	[ ([t|Word32|], "width")
	, ([t|Word32|], "height")
	, ([t|Word32|], "depth")
	]


-- allocation

genEnum [t|Word32|] "VkSystemAllocationScope"
	[ ("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND", 0)
	, ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT", 1)
	, ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE", 2)
	, ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE", 3)
	, ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE", 4)
	]

genEnum [t|Word32|] "VkInternalAllocationType"
	[ ("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE", 0)
	]

type FN_vkAllocationFunction =  Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())
type FN_vkReallocationFunction = Ptr () -> Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())
type FN_vkFreeFunction = Ptr () -> Ptr () -> IO ()
type FN_vkInternalAllocationNotification = Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()
type FN_vkInternalFreeNotification = Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()

genStruct "VkAllocationCallbacks"
	[ ([t|Ptr ()|], "pUserData")
	, ([t|FunPtr FN_vkAllocationFunction|], "pfnAllocation")
	, ([t|FunPtr FN_vkReallocationFunction|], "pfnReallocation")
	, ([t|FunPtr FN_vkFreeFunction|], "pfnFree")
	, ([t|FunPtr FN_vkInternalAllocationNotification|], "pfnInternalAllocation")
	, ([t|FunPtr FN_vkInternalFreeNotification|], "pfnInternalFree")
	]


-- instance

vkDefineHandle "VkInstance"

genStruct "VkApplicationInfo"
	[ ([t|VkStructureType|], "sType")
	, ([t|Ptr ()|], "pNext")
	, ([t|Ptr CChar|], "pApplicationName")
	, ([t|Word32|], "applicationVersion")
	, ([t|Ptr CChar|], "pEngineName")
	, ([t|Word32|], "engineVersion")
	, ([t|Word32|], "apiVersion")
	]

type VkInstanceCreateFlags = VkFlags

genStruct "VkInstanceCreateInfo"
	[ ([t|VkStructureType|], "sType")
	, ([t|Ptr ()|], "pNext")
	, ([t|VkInstanceCreateFlags|], "flags")
	, ([t|Ptr VkApplicationInfo|], "pApplicationInfo")
	, ([t|Word32|], "enabledLayerCount")
	, ([t|Ptr (Ptr CChar)|], "ppEnabledLayerNames")
	, ([t|Word32|], "enabledExtensionCount")
	, ([t|Ptr (Ptr CChar)|], "ppEnabledExtensionNames")
	]

foreign import VKAPI_CALL safe vkCreateInstance
	:: Ptr VkInstanceCreateInfo
	-> Ptr VkAllocationCallbacks
	-> Ptr VkInstance
	-> IO Word32 -- VkResult

foreign import VKAPI_CALL safe vkDestroyInstance
	:: VkInstance
	-> Ptr VkAllocationCallbacks
	-> IO ()

foreign import VKAPI_CALL safe vkGetInstanceProcAddr
	:: VkInstance
	-> Ptr CChar
	-> IO (FunPtr (IO ()))


-- physical device

vkDefineHandle "VkPhysicalDevice"

type FN_vkEnumeratePhysicalDevices
	=  VkInstance
	-> Ptr Word32
	-> Ptr VkPhysicalDevice
	-> IO Word32 -- VkResult

genEnum [t|Word32|] "VkPhysicalDeviceType"
	[ ("VK_PHYSICAL_DEVICE_TYPE_OTHER", 0)
	, ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", 1)
	, ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU", 2)
	, ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU", 3)
	, ("VK_PHYSICAL_DEVICE_TYPE_CPU", 4)
	]

type VkDeviceSize = Word64
type VkSampleCountFlags = VkFlags

genStruct "VkPhysicalDeviceLimits"
	[ ([t|Word32|], "maxImageDimension1D")
	, ([t|Word32|], "maxImageDimension2D")
	, ([t|Word32|], "maxImageDimension3D")
	, ([t|Word32|], "maxImageDimensionCube")
	, ([t|Word32|], "maxImageArrayLayers")
	, ([t|Word32|], "maxTexelBufferElements")
	, ([t|Word32|], "maxUniformBufferRange")
	, ([t|Word32|], "maxStorageBufferRange")
	, ([t|Word32|], "maxPushConstantsSize")
	, ([t|Word32|], "maxMemoryAllocationCount")
	, ([t|Word32|], "maxSamplerAllocationCount")
	, ([t|VkDeviceSize|], "bufferImageGranularity")
	, ([t|VkDeviceSize|], "sparseAddressSpaceSize")
	, ([t|Word32|], "maxBoundDescriptorSets")
	, ([t|Word32|], "maxPerStageDescriptorSamplers")
	, ([t|Word32|], "maxPerStageDescriptorUniformBuffers")
	, ([t|Word32|], "maxPerStageDescriptorStorageBuffers")
	, ([t|Word32|], "maxPerStageDescriptorSampledImages")
	, ([t|Word32|], "maxPerStageDescriptorStorageImages")
	, ([t|Word32|], "maxPerStageDescriptorInputAttachments")
	, ([t|Word32|], "maxPerStageResources")
	, ([t|Word32|], "maxDescriptorSetSamplers")
	, ([t|Word32|], "maxDescriptorSetUniformBuffers")
	, ([t|Word32|], "maxDescriptorSetUniformBuffersDynamic")
	, ([t|Word32|], "maxDescriptorSetStorageBuffers")
	, ([t|Word32|], "maxDescriptorSetStorageBuffersDynamic")
	, ([t|Word32|], "maxDescriptorSetSampledImages")
	, ([t|Word32|], "maxDescriptorSetStorageImages")
	, ([t|Word32|], "maxDescriptorSetInputAttachments")
	, ([t|Word32|], "maxVertexInputAttributes")
	, ([t|Word32|], "maxVertexInputBindings")
	, ([t|Word32|], "maxVertexInputAttributeOffset")
	, ([t|Word32|], "maxVertexInputBindingStride")
	, ([t|Word32|], "maxVertexOutputComponents")
	, ([t|Word32|], "maxTessellationGenerationLevel")
	, ([t|Word32|], "maxTessellationPatchSize")
	, ([t|Word32|], "maxTessellationControlPerVertexInputComponents")
	, ([t|Word32|], "maxTessellationControlPerVertexOutputComponents")
	, ([t|Word32|], "maxTessellationControlPerPatchOutputComponents")
	, ([t|Word32|], "maxTessellationControlTotalOutputComponents")
	, ([t|Word32|], "maxTessellationEvaluationInputComponents")
	, ([t|Word32|], "maxTessellationEvaluationOutputComponents")
	, ([t|Word32|], "maxGeometryShaderInvocations")
	, ([t|Word32|], "maxGeometryInputComponents")
	, ([t|Word32|], "maxGeometryOutputComponents")
	, ([t|Word32|], "maxGeometryOutputVertices")
	, ([t|Word32|], "maxGeometryTotalOutputComponents")
	, ([t|Word32|], "maxFragmentInputComponents")
	, ([t|Word32|], "maxFragmentOutputAttachments")
	, ([t|Word32|], "maxFragmentDualSrcAttachments")
	, ([t|Word32|], "maxFragmentCombinedOutputResources")
	, ([t|Word32|], "maxComputeSharedMemorySize")
	, ([t|Word32|], "maxComputeWorkGroupCount0")
	, ([t|Word32|], "maxComputeWorkGroupCount1")
	, ([t|Word32|], "maxComputeWorkGroupCount2")
	, ([t|Word32|], "maxComputeWorkGroupInvocations")
	, ([t|Word32|], "maxComputeWorkGroupSize0")
	, ([t|Word32|], "maxComputeWorkGroupSize1")
	, ([t|Word32|], "maxComputeWorkGroupSize2")
	, ([t|Word32|], "subPixelPrecisionBits")
	, ([t|Word32|], "subTexelPrecisionBits")
	, ([t|Word32|], "mipmapPrecisionBits")
	, ([t|Word32|], "maxDrawIndexedIndexValue")
	, ([t|Word32|], "maxDrawIndirectCount")
	, ([t|Float|], "maxSamplerLodBias")
	, ([t|Float|], "maxSamplerAnisotropy")
	, ([t|Word32|], "maxViewports")
	, ([t|Word32|], "maxViewportDimensions0")
	, ([t|Word32|], "maxViewportDimensions1")
	, ([t|Float|], "viewportBoundsRange0")
	, ([t|Float|], "viewportBoundsRange1")
	, ([t|Word32|], "viewportSubPixelBits")
	, ([t|CSize|], "minMemoryMapAlignment")
	, ([t|VkDeviceSize|], "minTexelBufferOffsetAlignment")
	, ([t|VkDeviceSize|], "minUniformBufferOffsetAlignment")
	, ([t|VkDeviceSize|], "minStorageBufferOffsetAlignment")
	, ([t|Int32|], "minTexelOffset")
	, ([t|Word32|], "maxTexelOffset")
	, ([t|Int32|], "minTexelGatherOffset")
	, ([t|Word32|], "maxTexelGatherOffset")
	, ([t|Float|], "minInterpolationOffset")
	, ([t|Float|], "maxInterpolationOffset")
	, ([t|Word32|], "subPixelInterpolationOffsetBits")
	, ([t|Word32|], "maxFramebufferWidth")
	, ([t|Word32|], "maxFramebufferHeight")
	, ([t|Word32|], "maxFramebufferLayers")
	, ([t|VkSampleCountFlags|], "framebufferColorSampleCounts")
	, ([t|VkSampleCountFlags|], "framebufferDepthSampleCounts")
	, ([t|VkSampleCountFlags|], "framebufferStencilSampleCounts")
	, ([t|VkSampleCountFlags|], "framebufferNoAttachmentsSampleCounts")
	, ([t|Word32|], "maxColorAttachments")
	, ([t|VkSampleCountFlags|], "sampledImageColorSampleCounts")
	, ([t|VkSampleCountFlags|], "sampledImageIntegerSampleCounts")
	, ([t|VkSampleCountFlags|], "sampledImageDepthSampleCounts")
	, ([t|VkSampleCountFlags|], "sampledImageStencilSampleCounts")
	, ([t|VkSampleCountFlags|], "storageImageSampleCounts")
	, ([t|Word32|], "maxSampleMaskWords")
	, ([t|VkBool32|], "timestampComputeAndGraphics")
	, ([t|Float|], "timestampPeriod")
	, ([t|Word32|], "maxClipDistances")
	, ([t|Word32|], "maxCullDistances")
	, ([t|Word32|], "maxCombinedClipAndCullDistances")
	, ([t|Word32|], "discreteQueuePriorities")
	, ([t|Float|], "pointSizeRange0")
	, ([t|Float|], "pointSizeRange1")
	, ([t|Float|], "lineWidthRange0")
	, ([t|Float|], "lineWidthRange1")
	, ([t|Float|], "pointSizeGranularity")
	, ([t|Float|], "lineWidthGranularity")
	, ([t|VkBool32|], "strictLines")
	, ([t|VkBool32|], "standardSampleLocations")
	, ([t|VkDeviceSize|], "optimalBufferCopyOffsetAlignment")
	, ([t|VkDeviceSize|], "optimalBufferCopyRowPitchAlignment")
	, ([t|VkDeviceSize|], "nonCoherentAtomSize")
	]

genStruct "VkPhysicalDeviceSparseProperties"
	[ ([t|VkBool32|], "residencyStandard2DBlockShape")
	, ([t|VkBool32|], "residencyStandard2DMultisampleBlockShape")
	, ([t|VkBool32|], "residencyStandard3DBlockShape")
	, ([t|VkBool32|], "residencyAlignedMipSize")
	, ([t|VkBool32|], "residencyNonResidentStrict")
	]

genStructWithArrays "VkPhysicalDeviceProperties"
	[ ([t|Word32|], "apiVersion", 0)
	, ([t|Word32|], "driverVersion", 0)
	, ([t|Word32|], "vendorID", 0)
	, ([t|Word32|], "deviceID", 0)
	, ([t|VkPhysicalDeviceType|], "deviceType", 0)
	, ([t|CChar|], "deviceName", 256 {-VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-})
	, ([t|Word8|], "pipelineCacheUUID", 16 {-VK_UUID_SIZE-})
	, ([t|VkPhysicalDeviceLimits|], "limits", 0)
	, ([t|VkPhysicalDeviceSparseProperties|], "sparseProperties", 0)
	]

type FN_vkGetPhysicalDeviceProperties
	=  VkPhysicalDevice
	-> Ptr VkPhysicalDeviceProperties
	-> IO ()

genStruct "VkPhysicalDeviceFeatures"
	[ ([t|VkBool32|], "robustBufferAccess")
	, ([t|VkBool32|], "fullDrawIndexUint32")
	, ([t|VkBool32|], "imageCubeArray")
	, ([t|VkBool32|], "independentBlend")
	, ([t|VkBool32|], "geometryShader")
	, ([t|VkBool32|], "tessellationShader")
	, ([t|VkBool32|], "sampleRateShading")
	, ([t|VkBool32|], "dualSrcBlend")
	, ([t|VkBool32|], "logicOp")
	, ([t|VkBool32|], "multiDrawIndirect")
	, ([t|VkBool32|], "drawIndirectFirstInstance")
	, ([t|VkBool32|], "depthClamp")
	, ([t|VkBool32|], "depthBiasClamp")
	, ([t|VkBool32|], "fillModeNonSolid")
	, ([t|VkBool32|], "depthBounds")
	, ([t|VkBool32|], "wideLines")
	, ([t|VkBool32|], "largePoints")
	, ([t|VkBool32|], "alphaToOne")
	, ([t|VkBool32|], "multiViewport")
	, ([t|VkBool32|], "samplerAnisotropy")
	, ([t|VkBool32|], "textureCompressionETC2")
	, ([t|VkBool32|], "textureCompressionASTC_LDR")
	, ([t|VkBool32|], "textureCompressionBC")
	, ([t|VkBool32|], "occlusionQueryPrecise")
	, ([t|VkBool32|], "pipelineStatisticsQuery")
	, ([t|VkBool32|], "vertexPipelineStoresAndAtomics")
	, ([t|VkBool32|], "fragmentStoresAndAtomics")
	, ([t|VkBool32|], "shaderTessellationAndGeometryPointSize")
	, ([t|VkBool32|], "shaderImageGatherExtended")
	, ([t|VkBool32|], "shaderStorageImageExtendedFormats")
	, ([t|VkBool32|], "shaderStorageImageMultisample")
	, ([t|VkBool32|], "shaderStorageImageReadWithoutFormat")
	, ([t|VkBool32|], "shaderStorageImageWriteWithoutFormat")
	, ([t|VkBool32|], "shaderUniformBufferArrayDynamicIndexing")
	, ([t|VkBool32|], "shaderSampledImageArrayDynamicIndexing")
	, ([t|VkBool32|], "shaderStorageBufferArrayDynamicIndexing")
	, ([t|VkBool32|], "shaderStorageImageArrayDynamicIndexing")
	, ([t|VkBool32|], "shaderClipDistance")
	, ([t|VkBool32|], "shaderCullDistance")
	, ([t|VkBool32|], "shaderFloat64")
	, ([t|VkBool32|], "shaderInt64")
	, ([t|VkBool32|], "shaderInt16")
	, ([t|VkBool32|], "shaderResourceResidency")
	, ([t|VkBool32|], "shaderResourceMinLod")
	, ([t|VkBool32|], "sparseBinding")
	, ([t|VkBool32|], "sparseResidencyBuffer")
	, ([t|VkBool32|], "sparseResidencyImage2D")
	, ([t|VkBool32|], "sparseResidencyImage3D")
	, ([t|VkBool32|], "sparseResidency2Samples")
	, ([t|VkBool32|], "sparseResidency4Samples")
	, ([t|VkBool32|], "sparseResidency8Samples")
	, ([t|VkBool32|], "sparseResidency16Samples")
	, ([t|VkBool32|], "sparseResidencyAliased")
	, ([t|VkBool32|], "variableMultisampleRate")
	, ([t|VkBool32|], "inheritedQueries")
	]

type FN_vkGetPhysicalDeviceFeatures
	=  VkPhysicalDevice
	-> Ptr VkPhysicalDeviceFeatures
	-> IO ()

type VkQueueFlags = VkFlags

genEnum [t|Word32|] "VkQueueFlagBits"
	[ ("VK_QUEUE_GRAPHICS_BIT", 1)
	, ("VK_QUEUE_COMPUTE_BIT", 2)
	, ("VK_QUEUE_TRANSFER_BIT", 4)
	, ("VK_QUEUE_SPARSE_BINDING_BIT", 8)
	]

genStruct "VkQueueFamilyProperties"
	[ ([t|VkQueueFlags|], "queueFlags")
	, ([t|Word32|], "queueCount")
	, ([t|Word32|], "timestampValidBits")
	, ([t|VkExtent3D|], "minImageTransferGranularity")
	]

type FN_vkGetPhysicalDeviceQueueFamilyProperties
	=  VkPhysicalDevice
	-> Ptr Word32
	-> Ptr VkQueueFamilyProperties
	-> IO ()


-- device

vkDefineHandle "VkDevice"

type VkDeviceCreateFlags = VkFlags
type VkDeviceQueueCreateFlags = VkFlags

genStruct "VkDeviceQueueCreateInfo"
	[ ([t|VkStructureType|], "sType")
	, ([t|Ptr ()|], "pNext")
	, ([t|VkDeviceQueueCreateFlags|], "flags")
	, ([t|Word32|], "queueFamilyIndex")
	, ([t|Word32|], "queueCount")
	, ([t|Ptr Float|], "pQueuePriorities")
	]

genStruct "VkDeviceCreateInfo"
	[ ([t|VkStructureType|], "sType")
	, ([t|Ptr ()|], "pNext")
	, ([t|VkDeviceCreateFlags|], "flags")
	, ([t|Word32|], "queueCreateInfoCount")
	, ([t|Ptr VkDeviceQueueCreateInfo|], "pQueueCreateInfos")
	, ([t|Word32|], "enabledLayerCount")
	, ([t|Ptr (Ptr CChar)|], "ppEnabledLayerNames")
	, ([t|Word32|], "enabledExtensionCount")
	, ([t|Ptr (Ptr CChar)|], "ppEnabledExtensionNames")
	, ([t|Ptr VkPhysicalDeviceFeatures|], "pEnabledFeatures")
	]

type FN_vkCreateDevice
	=  VkPhysicalDevice
	-> Ptr VkDeviceCreateInfo
	-> Ptr VkAllocationCallbacks
	-> Ptr VkDevice
	-> IO Word32 -- VkResult

type FN_vkDestroyDevice
	=  VkDevice
	-> Ptr VkAllocationCallbacks
	-> IO ()


-- queue

vkDefineHandle "VkQueue"

type FN_vkGetDeviceQueue
	=  VkDevice
	-> Word32
	-> Word32
	-> Ptr VkQueue
	-> IO ()
