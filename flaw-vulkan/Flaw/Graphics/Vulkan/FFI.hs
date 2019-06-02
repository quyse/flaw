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
  , VkOffset2D(..)
  , VkExtent2D(..)
  , VkExtent3D(..)
  , VkRect2D(..)
  -- * allocation
  , VkSystemAllocationScope(..)
  , VkInternalAllocationType(..)
  , FN_vkAllocationFunction
  , FN_vkReallocationFunction
  , FN_vkFreeFunction
  , FN_vkInternalAllocationNotification
  , FN_vkInternalFreeNotification
  , VkAllocationCallbacks(..)
  -- * format
  , VkFormat(..)
  , VkSampleCountFlags
  , VkSampleCountFlagBits(..)
  , VkSampleMask
  -- * instance
  , VkInstance
  , VkApplicationInfo(..)
  , VkInstanceCreateFlags
  , VkInstanceCreateInfo(..)
  , vkCreateInstance
  , vkDestroyInstance
  , vkGetInstanceProcAddr
  , vkGetInstanceProc
  , vkGetDeviceProc
  -- * physical device
  , VkPhysicalDevice
  , FN_vkEnumeratePhysicalDevices
  , VkPhysicalDeviceType(..)
  , VkDeviceSize
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
  , FN_vkGetDeviceProcAddr
  , VkDeviceCreateFlags
  , VkDeviceQueueCreateFlags
  , VkDeviceQueueCreateInfo(..)
  , VkDeviceCreateInfo(..)
  , FN_vkCreateDevice
  , FN_vkDestroyDevice
  -- * queue
  , VkQueue
  , FN_vkGetDeviceQueue
  -- * image
  , VkImage
  , VkImageLayout(..)
  , VkImageCreateInfo(..)
  , FN_vkCreateImage
  , FN_vkDestroyImage
  -- * image view
  , VkImageView
  , VkImageViewCreateFlags
  , VkImageViewType(..)
  , VkComponentSwizzle(..)
  , VkComponentMapping(..)
  , VkImageViewCreateInfo(..)
  , FN_vkCreateImageView
  , FN_vkDestroyImageView
  -- * render pass
  , VkRenderPass
  , VkRenderPassCreateFlags
  , VkAttachmentDescriptionFlags
  , VkAttachmentDescriptionFlagBits(..)
  , VkAttachmentLoadOp(..)
  , VkAttachmentStoreOp(..)
  , VkAttachmentDescription(..)
  , VkSubpassDescriptionFlags
  , VkSubpassDescriptionFlagBits(..)
  , VkPipelineBindPoint(..)
  , VkAttachmentReference(..)
  , VkSubpassDescription(..)
  , VkPipelineStageFlags
  , VkPipelineStageFlagBits(..)
  , VkAccessFlags
  , VkAccessFlagBits(..)
  , VkDependencyFlags
  , VkDependencyFlagBits(..)
  , VkSubpassDependency(..)
  , VkRenderPassCreateInfo(..)
  , FN_vkCreateRenderPass
  , FN_vkDestroyRenderPass
  -- * framebuffer
  , VkFramebuffer
  , VkFramebufferCreateFlags
  , VkFramebufferCreateInfo(..)
  , FN_vkCreateFramebuffer
  , FN_vkDestroyFramebuffer
  -- * shader
  , VkShaderModule
  , VkShaderModuleCreateFlags
  , VkShaderModuleCreateInfo(..)
  , FN_vkCreateShaderModule
  , FN_vkDestroyShaderModule
  -- * pipeline
  , VkPipeline
  , VkPipelineLayout
  , VkPipelineCache
  , VkPipelineCreateFlags
  , VkPipelineCreateFlagBits(..)
  , VkPipelineShaderStageCreateFlags
  , VkShaderStageFlagBits(..)
  , VkSpecializationMapEntry(..)
  , VkSpecializationInfo(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineVertexInputStateCreateFlags
  , VkVertexInputRate(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputAttributeDescription(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags
  , VkPrimitiveTopology(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineTessellationStateCreateFlags
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineViewportStateCreateFlags
  , VkViewport(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateFlags
  , VkPolygonMode(..)
  , VkCullModeFlags
  , VkCullModeFlagBits(..)
  , VkFrontFace(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateFlags
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineDepthStencilStateCreateFlags
  , VkCompareOp(..)
  , VkStencilOp(..)
  , VkStencilOpState(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineColorBlendStateCreateFlags
  , VkBlendFactor(..)
  , VkBlendOp(..)
  , VkColorComponentFlags
  , VkColorComponentFlagBits(..)
  , VkLogicOp(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags
  , VkDynamicState(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkComputePipelineCreateInfo(..)
  , FN_vkCreateGraphicsPipelines
  , FN_vkCreateComputePipelines
  , FN_vkDestroyPipeline
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

genStruct "VkOffset2D"
  [ ([t|Int32|], "x")
  , ([t|Int32|], "y")
  ]

genStruct "VkExtent2D"
  [ ([t|Word32|], "width")
  , ([t|Word32|], "height")
  ]

genStruct "VkExtent3D"
  [ ([t|Word32|], "width")
  , ([t|Word32|], "height")
  , ([t|Word32|], "depth")
  ]

genStruct "VkRect2D"
  [ ([t|VkOffset2D|], "offset")
  , ([t|VkExtent2D|], "extent")
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


-- format

genEnum [t|Word32|] "VkFormat"
  [ ("VK_FORMAT_UNDEFINED", 0)
  , ("VK_FORMAT_R4G4_UNORM_PACK8", 1)
  , ("VK_FORMAT_R4G4B4A4_UNORM_PACK16", 2)
  , ("VK_FORMAT_B4G4R4A4_UNORM_PACK16", 3)
  , ("VK_FORMAT_R5G6B5_UNORM_PACK16", 4)
  , ("VK_FORMAT_B5G6R5_UNORM_PACK16", 5)
  , ("VK_FORMAT_R5G5B5A1_UNORM_PACK16", 6)
  , ("VK_FORMAT_B5G5R5A1_UNORM_PACK16", 7)
  , ("VK_FORMAT_A1R5G5B5_UNORM_PACK16", 8)
  , ("VK_FORMAT_R8_UNORM", 9)
  , ("VK_FORMAT_R8_SNORM", 10)
  , ("VK_FORMAT_R8_USCALED", 11)
  , ("VK_FORMAT_R8_SSCALED", 12)
  , ("VK_FORMAT_R8_UINT", 13)
  , ("VK_FORMAT_R8_SINT", 14)
  , ("VK_FORMAT_R8_SRGB", 15)
  , ("VK_FORMAT_R8G8_UNORM", 16)
  , ("VK_FORMAT_R8G8_SNORM", 17)
  , ("VK_FORMAT_R8G8_USCALED", 18)
  , ("VK_FORMAT_R8G8_SSCALED", 19)
  , ("VK_FORMAT_R8G8_UINT", 20)
  , ("VK_FORMAT_R8G8_SINT", 21)
  , ("VK_FORMAT_R8G8_SRGB", 22)
  , ("VK_FORMAT_R8G8B8_UNORM", 23)
  , ("VK_FORMAT_R8G8B8_SNORM", 24)
  , ("VK_FORMAT_R8G8B8_USCALED", 25)
  , ("VK_FORMAT_R8G8B8_SSCALED", 26)
  , ("VK_FORMAT_R8G8B8_UINT", 27)
  , ("VK_FORMAT_R8G8B8_SINT", 28)
  , ("VK_FORMAT_R8G8B8_SRGB", 29)
  , ("VK_FORMAT_B8G8R8_UNORM", 30)
  , ("VK_FORMAT_B8G8R8_SNORM", 31)
  , ("VK_FORMAT_B8G8R8_USCALED", 32)
  , ("VK_FORMAT_B8G8R8_SSCALED", 33)
  , ("VK_FORMAT_B8G8R8_UINT", 34)
  , ("VK_FORMAT_B8G8R8_SINT", 35)
  , ("VK_FORMAT_B8G8R8_SRGB", 36)
  , ("VK_FORMAT_R8G8B8A8_UNORM", 37)
  , ("VK_FORMAT_R8G8B8A8_SNORM", 38)
  , ("VK_FORMAT_R8G8B8A8_USCALED", 39)
  , ("VK_FORMAT_R8G8B8A8_SSCALED", 40)
  , ("VK_FORMAT_R8G8B8A8_UINT", 41)
  , ("VK_FORMAT_R8G8B8A8_SINT", 42)
  , ("VK_FORMAT_R8G8B8A8_SRGB", 43)
  , ("VK_FORMAT_B8G8R8A8_UNORM", 44)
  , ("VK_FORMAT_B8G8R8A8_SNORM", 45)
  , ("VK_FORMAT_B8G8R8A8_USCALED", 46)
  , ("VK_FORMAT_B8G8R8A8_SSCALED", 47)
  , ("VK_FORMAT_B8G8R8A8_UINT", 48)
  , ("VK_FORMAT_B8G8R8A8_SINT", 49)
  , ("VK_FORMAT_B8G8R8A8_SRGB", 50)
  , ("VK_FORMAT_A8B8G8R8_UNORM_PACK32", 51)
  , ("VK_FORMAT_A8B8G8R8_SNORM_PACK32", 52)
  , ("VK_FORMAT_A8B8G8R8_USCALED_PACK32", 53)
  , ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32", 54)
  , ("VK_FORMAT_A8B8G8R8_UINT_PACK32", 55)
  , ("VK_FORMAT_A8B8G8R8_SINT_PACK32", 56)
  , ("VK_FORMAT_A8B8G8R8_SRGB_PACK32", 57)
  , ("VK_FORMAT_A2R10G10B10_UNORM_PACK32", 58)
  , ("VK_FORMAT_A2R10G10B10_SNORM_PACK32", 59)
  , ("VK_FORMAT_A2R10G10B10_USCALED_PACK32", 60)
  , ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32", 61)
  , ("VK_FORMAT_A2R10G10B10_UINT_PACK32", 62)
  , ("VK_FORMAT_A2R10G10B10_SINT_PACK32", 63)
  , ("VK_FORMAT_A2B10G10R10_UNORM_PACK32", 64)
  , ("VK_FORMAT_A2B10G10R10_SNORM_PACK32", 65)
  , ("VK_FORMAT_A2B10G10R10_USCALED_PACK32", 66)
  , ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32", 67)
  , ("VK_FORMAT_A2B10G10R10_UINT_PACK32", 68)
  , ("VK_FORMAT_A2B10G10R10_SINT_PACK32", 69)
  , ("VK_FORMAT_R16_UNORM", 70)
  , ("VK_FORMAT_R16_SNORM", 71)
  , ("VK_FORMAT_R16_USCALED", 72)
  , ("VK_FORMAT_R16_SSCALED", 73)
  , ("VK_FORMAT_R16_UINT", 74)
  , ("VK_FORMAT_R16_SINT", 75)
  , ("VK_FORMAT_R16_SFLOAT", 76)
  , ("VK_FORMAT_R16G16_UNORM", 77)
  , ("VK_FORMAT_R16G16_SNORM", 78)
  , ("VK_FORMAT_R16G16_USCALED", 79)
  , ("VK_FORMAT_R16G16_SSCALED", 80)
  , ("VK_FORMAT_R16G16_UINT", 81)
  , ("VK_FORMAT_R16G16_SINT", 82)
  , ("VK_FORMAT_R16G16_SFLOAT", 83)
  , ("VK_FORMAT_R16G16B16_UNORM", 84)
  , ("VK_FORMAT_R16G16B16_SNORM", 85)
  , ("VK_FORMAT_R16G16B16_USCALED", 86)
  , ("VK_FORMAT_R16G16B16_SSCALED", 87)
  , ("VK_FORMAT_R16G16B16_UINT", 88)
  , ("VK_FORMAT_R16G16B16_SINT", 89)
  , ("VK_FORMAT_R16G16B16_SFLOAT", 90)
  , ("VK_FORMAT_R16G16B16A16_UNORM", 91)
  , ("VK_FORMAT_R16G16B16A16_SNORM", 92)
  , ("VK_FORMAT_R16G16B16A16_USCALED", 93)
  , ("VK_FORMAT_R16G16B16A16_SSCALED", 94)
  , ("VK_FORMAT_R16G16B16A16_UINT", 95)
  , ("VK_FORMAT_R16G16B16A16_SINT", 96)
  , ("VK_FORMAT_R16G16B16A16_SFLOAT", 97)
  , ("VK_FORMAT_R32_UINT", 98)
  , ("VK_FORMAT_R32_SINT", 99)
  , ("VK_FORMAT_R32_SFLOAT", 100)
  , ("VK_FORMAT_R32G32_UINT", 101)
  , ("VK_FORMAT_R32G32_SINT", 102)
  , ("VK_FORMAT_R32G32_SFLOAT", 103)
  , ("VK_FORMAT_R32G32B32_UINT", 104)
  , ("VK_FORMAT_R32G32B32_SINT", 105)
  , ("VK_FORMAT_R32G32B32_SFLOAT", 106)
  , ("VK_FORMAT_R32G32B32A32_UINT", 107)
  , ("VK_FORMAT_R32G32B32A32_SINT", 108)
  , ("VK_FORMAT_R32G32B32A32_SFLOAT", 109)
  , ("VK_FORMAT_R64_UINT", 110)
  , ("VK_FORMAT_R64_SINT", 111)
  , ("VK_FORMAT_R64_SFLOAT", 112)
  , ("VK_FORMAT_R64G64_UINT", 113)
  , ("VK_FORMAT_R64G64_SINT", 114)
  , ("VK_FORMAT_R64G64_SFLOAT", 115)
  , ("VK_FORMAT_R64G64B64_UINT", 116)
  , ("VK_FORMAT_R64G64B64_SINT", 117)
  , ("VK_FORMAT_R64G64B64_SFLOAT", 118)
  , ("VK_FORMAT_R64G64B64A64_UINT", 119)
  , ("VK_FORMAT_R64G64B64A64_SINT", 120)
  , ("VK_FORMAT_R64G64B64A64_SFLOAT", 121)
  , ("VK_FORMAT_B10G11R11_UFLOAT_PACK32", 122)
  , ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32", 123)
  , ("VK_FORMAT_D16_UNORM", 124)
  , ("VK_FORMAT_X8_D24_UNORM_PACK32", 125)
  , ("VK_FORMAT_D32_SFLOAT", 126)
  , ("VK_FORMAT_S8_UINT", 127)
  , ("VK_FORMAT_D16_UNORM_S8_UINT", 128)
  , ("VK_FORMAT_D24_UNORM_S8_UINT", 129)
  , ("VK_FORMAT_D32_SFLOAT_S8_UINT", 130)
  , ("VK_FORMAT_BC1_RGB_UNORM_BLOCK", 131)
  , ("VK_FORMAT_BC1_RGB_SRGB_BLOCK", 132)
  , ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK", 133)
  , ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK", 134)
  , ("VK_FORMAT_BC2_UNORM_BLOCK", 135)
  , ("VK_FORMAT_BC2_SRGB_BLOCK", 136)
  , ("VK_FORMAT_BC3_UNORM_BLOCK", 137)
  , ("VK_FORMAT_BC3_SRGB_BLOCK", 138)
  , ("VK_FORMAT_BC4_UNORM_BLOCK", 139)
  , ("VK_FORMAT_BC4_SNORM_BLOCK", 140)
  , ("VK_FORMAT_BC5_UNORM_BLOCK", 141)
  , ("VK_FORMAT_BC5_SNORM_BLOCK", 142)
  , ("VK_FORMAT_BC6H_UFLOAT_BLOCK", 143)
  , ("VK_FORMAT_BC6H_SFLOAT_BLOCK", 144)
  , ("VK_FORMAT_BC7_UNORM_BLOCK", 145)
  , ("VK_FORMAT_BC7_SRGB_BLOCK", 146)
  , ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK", 147)
  , ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK", 148)
  , ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK", 149)
  , ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK", 150)
  , ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK", 151)
  , ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK", 152)
  , ("VK_FORMAT_EAC_R11_UNORM_BLOCK", 153)
  , ("VK_FORMAT_EAC_R11_SNORM_BLOCK", 154)
  , ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK", 155)
  , ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK", 156)
  , ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK", 157)
  , ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK", 158)
  , ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK", 159)
  , ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK", 160)
  , ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK", 161)
  , ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK", 162)
  , ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK", 163)
  , ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK", 164)
  , ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK", 165)
  , ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK", 166)
  , ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK", 167)
  , ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK", 168)
  , ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK", 169)
  , ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK", 170)
  , ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK", 171)
  , ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK", 172)
  , ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK", 173)
  , ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK", 174)
  , ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK", 175)
  , ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK", 176)
  , ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK", 177)
  , ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK", 178)
  , ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK", 179)
  , ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK", 180)
  , ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK", 181)
  , ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK", 182)
  , ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK", 183)
  , ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK", 184)
  , ("VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG", 1000054000)
  , ("VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG", 1000054001)
  , ("VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG", 1000054002)
  , ("VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG", 1000054003)
  , ("VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG", 1000054004)
  , ("VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG", 1000054005)
  , ("VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG", 1000054006)
  , ("VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG", 1000054007)
  ]

type VkSampleCountFlags = VkFlags
genEnum [t|Word32|] "VkSampleCountFlagBits"
  [ ("VK_SAMPLE_COUNT_1_BIT", 0x00000001)
  , ("VK_SAMPLE_COUNT_2_BIT", 0x00000002)
  , ("VK_SAMPLE_COUNT_4_BIT", 0x00000004)
  , ("VK_SAMPLE_COUNT_8_BIT", 0x00000008)
  , ("VK_SAMPLE_COUNT_16_BIT", 0x00000010)
  , ("VK_SAMPLE_COUNT_32_BIT", 0x00000020)
  , ("VK_SAMPLE_COUNT_64_BIT", 0x00000040)
  ]

type VkSampleMask = Word32


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

type FN_vkGetDeviceProcAddr
  =  VkDevice
  -> Ptr CChar
  -> IO (FunPtr (IO ()))

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


-- image

vkDefineHandle "VkImage"

genEnum [t|Word32|] "VkImageLayout"
  [ ("VK_IMAGE_LAYOUT_UNDEFINED", 0)
  , ("VK_IMAGE_LAYOUT_GENERAL", 1)
  , ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL", 2)
  , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL", 3)
  , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL", 4)
  , ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL", 5)
  , ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL", 6)
  , ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL", 7)
  , ("VK_IMAGE_LAYOUT_PREINITIALIZED", 8)
  , ("VK_IMAGE_LAYOUT_PRESENT_SRC_KHR", 1000001002)
  ]

type VkImageCreateFlags = VkFlags
genEnum [t|Word32|] "VkImageCreateFlagBits"
  [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT", 0x00000001)
  , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", 0x00000002)
  , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT", 0x00000004)
  , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT", 0x00000008)
  , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT", 0x00000010)
  , ("VK_IMAGE_CREATE_BIND_SFR_BIT_KHX", 0x00000040)
  , ("VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR", 0x00000020)
  ]

genEnum [t|Word32|] "VkImageType"
  [ ("VK_IMAGE_TYPE_1D", 0)
  , ("VK_IMAGE_TYPE_2D", 1)
  , ("VK_IMAGE_TYPE_3D", 2)
  ]

genEnum [t|Word32|] "VkImageTiling"
  [ ("VK_IMAGE_TILING_OPTIMAL", 0)
  , ("VK_IMAGE_TILING_LINEAR", 1)
  ]

type VkImageUsageFlags = VkFlags
genEnum [t|Word32|] "VkImageUsageFlagBits"
  [ ("VK_IMAGE_USAGE_TRANSFER_SRC_BIT", 0x00000001)
  , ("VK_IMAGE_USAGE_TRANSFER_DST_BIT", 0x00000002)
  , ("VK_IMAGE_USAGE_SAMPLED_BIT", 0x00000004)
  , ("VK_IMAGE_USAGE_STORAGE_BIT", 0x00000008)
  , ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT", 0x00000010)
  , ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT", 0x00000020)
  , ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT", 0x00000040)
  , ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT", 0x00000080)
  ]

genEnum [t|Word32|] "VkSharingMode"
  [ ("VK_SHARING_MODE_EXCLUSIVE", 0)
  , ("VK_SHARING_MODE_CONCURRENT", 1)
  ]

genStruct "VkImageCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkImageCreateFlags|], "flags")
  , ([t|VkImageType|], "imageType")
  , ([t|VkFormat|], "format")
  , ([t|VkExtent3D|], "extent")
  , ([t|Word32|], "mipLevels")
  , ([t|Word32|], "arrayLayers")
  , ([t|VkSampleCountFlagBits|], "samples")
  , ([t|VkImageTiling|], "tiling")
  , ([t|VkImageUsageFlags|], "usage")
  , ([t|VkSharingMode|], "sharingMode")
  , ([t|Word32|], "queueFamilyIndexCount")
  , ([t|Ptr Word32|], "pQueueFamilyIndices")
  , ([t|VkImageLayout|], "initialLayout")
  ]

type FN_vkCreateImage
  =  VkDevice
  -> Ptr VkImageCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkImage
  -> IO Word32 -- VkResult

type FN_vkDestroyImage
  =  VkDevice
  -> VkImage
  -> Ptr VkAllocationCallbacks
  -> IO ()


-- image view

vkDefineHandle "VkImageView"

type VkImageViewCreateFlags = VkFlags

genEnum [t|Word32|] "VkImageViewType"
  [ ("VK_IMAGE_VIEW_TYPE_1D", 0)
  , ("VK_IMAGE_VIEW_TYPE_2D", 1)
  , ("VK_IMAGE_VIEW_TYPE_3D", 2)
  , ("VK_IMAGE_VIEW_TYPE_CUBE", 3)
  , ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", 4)
  , ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", 5)
  , ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY", 6)
  ]

genEnum [t|Word32|] "VkComponentSwizzle"
  [ ("VK_COMPONENT_SWIZZLE_IDENTITY", 0)
  , ("VK_COMPONENT_SWIZZLE_ZERO", 1)
  , ("VK_COMPONENT_SWIZZLE_ONE", 2)
  , ("VK_COMPONENT_SWIZZLE_R", 3)
  , ("VK_COMPONENT_SWIZZLE_G", 4)
  , ("VK_COMPONENT_SWIZZLE_B", 5)
  , ("VK_COMPONENT_SWIZZLE_A", 6)
  ]

genStruct "VkComponentMapping"
  [ ([t|VkComponentSwizzle|], "r")
  , ([t|VkComponentSwizzle|], "g")
  , ([t|VkComponentSwizzle|], "b")
  , ([t|VkComponentSwizzle|], "a")
  ]

type VkImageAspectFlags = VkFlags
genEnum [t|Word32|] "VkImageAspectFlagBits"
  [ ("VK_IMAGE_ASPECT_COLOR_BIT", 0x00000001)
  , ("VK_IMAGE_ASPECT_DEPTH_BIT", 0x00000002)
  , ("VK_IMAGE_ASPECT_STENCIL_BIT", 0x00000004)
  , ("VK_IMAGE_ASPECT_METADATA_BIT", 0x00000008)
  ]

genStruct "VkImageSubresourceRange"
  [ ([t|VkImageAspectFlags|], "aspectMask")
  , ([t|Word32|], "baseMipLevel")
  , ([t|Word32|], "levelCount")
  , ([t|Word32|], "baseArrayLayer")
  , ([t|Word32|], "layerCount")
  ]

genStruct "VkImageViewCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkImageViewCreateFlags|], "flags")
  , ([t|VkImage|], "image")
  , ([t|VkImageViewType|], "viewType")
  , ([t|VkFormat|], "format")
  , ([t|VkComponentMapping|], "components")
  , ([t|VkImageSubresourceRange|], "subresourceRange")
  ]

type FN_vkCreateImageView
  =  VkDevice
  -> Ptr VkImageViewCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkImageView
  -> IO Word32 -- VkResult

type FN_vkDestroyImageView
  =  VkDevice
  -> VkImageView
  -> Ptr VkAllocationCallbacks
  -> IO ()


-- render pass

vkDefineHandle "VkRenderPass"

type VkRenderPassCreateFlags = VkFlags

type VkAttachmentDescriptionFlags = VkFlags
genEnum [t|Word32|] "VkAttachmentDescriptionFlagBits"
  [ ("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT", 0x00000001)
  , ("VK_ATTACHMENT_DESCRIPTION_FLAG_BITS_MAX_ENUM", 0x7FFFFFFF)
  ]

genEnum [t|Word32|] "VkAttachmentLoadOp"
  [ ("VK_ATTACHMENT_LOAD_OP_LOAD", 0)
  , ("VK_ATTACHMENT_LOAD_OP_CLEAR", 1)
  , ("VK_ATTACHMENT_LOAD_OP_DONT_CARE", 2)
  ]

genEnum [t|Word32|] "VkAttachmentStoreOp"
  [ ("VK_ATTACHMENT_STORE_OP_STORE", 0)
  , ("VK_ATTACHMENT_STORE_OP_DONT_CARE", 1)
  ]

genStruct "VkAttachmentDescription"
  [ ([t|VkAttachmentDescriptionFlags|], "flags")
  , ([t|VkFormat|], "format")
  , ([t|VkSampleCountFlagBits|], "samples")
  , ([t|VkAttachmentLoadOp|], "loadOp")
  , ([t|VkAttachmentStoreOp|], "storeOp")
  , ([t|VkAttachmentLoadOp|], "stencilLoadOp")
  , ([t|VkAttachmentStoreOp|], "stencilStoreOp")
  , ([t|VkImageLayout|], "initialLayout")
  , ([t|VkImageLayout|], "finalLayout")
  ]

type VkSubpassDescriptionFlags = VkFlags
genEnum [t|Word32|] "VkSubpassDescriptionFlagBits"
  [ ("VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX", 0x00000001)
  , ("VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX", 0x00000002)
  ]

genEnum [t|Word32|] "VkPipelineBindPoint"
  [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", 0)
  , ("VK_PIPELINE_BIND_POINT_COMPUTE", 1)
  ]

genStruct "VkAttachmentReference"
  [ ([t|Word32|], "attachment")
  , ([t|VkImageLayout|], "layout")
  ]

genStruct "VkSubpassDescription"
  [ ([t|VkSubpassDescriptionFlags|], "flags")
  , ([t|VkPipelineBindPoint|], "pipelineBindPoint")
  , ([t|Word32|], "inputAttachmentCount")
  , ([t|Ptr VkAttachmentReference|], "pInputAttachments")
  , ([t|Word32|], "colorAttachmentCount")
  , ([t|Ptr VkAttachmentReference|], "pColorAttachments")
  , ([t|Ptr VkAttachmentReference|], "pResolveAttachments")
  , ([t|Ptr VkAttachmentReference|], "pDepthStencilAttachment")
  , ([t|Word32|], "preserveAttachmentCount")
  , ([t|Ptr Word32|], "pPreserveAttachments")
  ]

type VkPipelineStageFlags = VkFlags
genEnum [t|Word32|] "VkPipelineStageFlagBits"
  [ ("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT", 0x00000001)
  , ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT", 0x00000002)
  , ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT", 0x00000004)
  , ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT", 0x00000008)
  , ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT", 0x00000010)
  , ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT", 0x00000020)
  , ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT", 0x00000040)
  , ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT", 0x00000080)
  , ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT", 0x00000100)
  , ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT", 0x00000200)
  , ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT", 0x00000400)
  , ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT", 0x00000800)
  , ("VK_PIPELINE_STAGE_TRANSFER_BIT", 0x00001000)
  , ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT", 0x00002000)
  , ("VK_PIPELINE_STAGE_HOST_BIT", 0x00004000)
  , ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT", 0x00008000)
  , ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT", 0x00010000)
  , ("VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX", 0x00020000)
  ]

type VkAccessFlags = VkFlags
genEnum [t|Word32|] "VkAccessFlagBits"
  [ ("VK_ACCESS_INDIRECT_COMMAND_READ_BIT", 0x00000001)
  , ("VK_ACCESS_INDEX_READ_BIT", 0x00000002)
  , ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT", 0x00000004)
  , ("VK_ACCESS_UNIFORM_READ_BIT", 0x00000008)
  , ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT", 0x00000010)
  , ("VK_ACCESS_SHADER_READ_BIT", 0x00000020)
  , ("VK_ACCESS_SHADER_WRITE_BIT", 0x00000040)
  , ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT", 0x00000080)
  , ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT", 0x00000100)
  , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT", 0x00000200)
  , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT", 0x00000400)
  , ("VK_ACCESS_TRANSFER_READ_BIT", 0x00000800)
  , ("VK_ACCESS_TRANSFER_WRITE_BIT", 0x00001000)
  , ("VK_ACCESS_HOST_READ_BIT", 0x00002000)
  , ("VK_ACCESS_HOST_WRITE_BIT", 0x00004000)
  , ("VK_ACCESS_MEMORY_READ_BIT", 0x00008000)
  , ("VK_ACCESS_MEMORY_WRITE_BIT", 0x00010000)
  , ("VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX", 0x00020000)
  , ("VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX", 0x00040000)
  , ("VK_ACCESS_FLAG_BITS_MAX_ENUM", 0x7FFFFFFF)
  ]

type VkDependencyFlags = VkFlags
genEnum [t|Word32|] "VkDependencyFlagBits"
  [ ("VK_DEPENDENCY_BY_REGION_BIT", 0x00000001)
  , ("VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX", 0x00000002)
  , ("VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX", 0x00000004)
  ]

genStruct "VkSubpassDependency"
  [ ([t|Word32|], "srcSubpass")
  , ([t|Word32|], "dstSubpass")
  , ([t|VkPipelineStageFlags|], "srcStageMask")
  , ([t|VkPipelineStageFlags|], "dstStageMask")
  , ([t|VkAccessFlags|], "srcAccessMask")
  , ([t|VkAccessFlags|], "dstAccessMask")
  , ([t|VkDependencyFlags|], "dependencyFlags")
  ]

genStruct "VkRenderPassCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkRenderPassCreateFlags|], "flags")
  , ([t|Word32|], "attachmentCount")
  , ([t|Ptr VkAttachmentDescription|], "pAttachments")
  , ([t|Word32|], "subpassCount")
  , ([t|Ptr VkSubpassDescription|], "pSubpasses")
  , ([t|Word32|], "dependencyCount")
  , ([t|Ptr VkSubpassDependency|], "pDependencies")
  ]

type FN_vkCreateRenderPass
  =  VkDevice
  -> Ptr VkRenderPassCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkRenderPass
  -> IO Word32 -- VkResult

type FN_vkDestroyRenderPass
  =  VkDevice
  -> VkRenderPass
  -> Ptr VkAllocationCallbacks
  -> IO ()


-- framebuffer

vkDefineHandle "VkFramebuffer"

type VkFramebufferCreateFlags = VkFlags

genStruct "VkFramebufferCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkFramebufferCreateFlags|], "flags")
  , ([t|VkRenderPass|], "renderPass")
  , ([t|Word32|], "attachmentCount")
  , ([t|Ptr VkImageView|], "pAttachments")
  , ([t|Word32|], "width")
  , ([t|Word32|], "height")
  , ([t|Word32|], "layers")
  ]

type FN_vkCreateFramebuffer
  =  VkDevice
  -> Ptr VkFramebufferCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkFramebuffer
  -> IO Word32 -- VkResult

type FN_vkDestroyFramebuffer
  =  VkDevice
  -> VkFramebuffer
  -> Ptr VkAllocationCallbacks
  -> IO ()


-- shader

vkDefineHandle "VkShaderModule"

type VkShaderModuleCreateFlags = VkFlags

genStruct "VkShaderModuleCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkShaderModuleCreateFlags|], "flags")
  , ([t|CSize|], "codeSize")
  , ([t|Ptr Word32|], "pCode")
  ]

type FN_vkCreateShaderModule
  =  VkDevice
  -> Ptr VkShaderModuleCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkShaderModule
  -> IO Word32 -- VkResult

type FN_vkDestroyShaderModule
  =  VkDevice
  -> VkShaderModule
  -> Ptr VkAllocationCallbacks
  -> IO ()


-- pipeline

vkDefineHandle "VkPipeline"
vkDefineHandle "VkPipelineLayout"
vkDefineHandle "VkPipelineCache"

type VkPipelineCreateFlags = VkFlags
genEnum [t|Word32|] "VkPipelineCreateFlagBits"
  [ ("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", 0x00000001)
  , ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT", 0x00000002)
  , ("VK_PIPELINE_CREATE_DERIVATIVE_BIT", 0x00000004)
  , ("VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX", 0x00000008)
  , ("VK_PIPELINE_CREATE_DISPATCH_BASE_KHX", 0x00000010)
  ]

type VkPipelineShaderStageCreateFlags = VkFlags

genEnum [t|Word32|] "VkShaderStageFlagBits"
  [ ("VK_SHADER_STAGE_VERTEX_BIT", 0x00000001)
  , ("VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT", 0x00000002)
  , ("VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT", 0x00000004)
  , ("VK_SHADER_STAGE_GEOMETRY_BIT", 0x00000008)
  , ("VK_SHADER_STAGE_FRAGMENT_BIT", 0x00000010)
  , ("VK_SHADER_STAGE_COMPUTE_BIT", 0x00000020)
  , ("VK_SHADER_STAGE_ALL_GRAPHICS", 0x0000001F)
  ]

genStruct "VkSpecializationMapEntry"
  [ ([t|Word32|], "constantID")
  , ([t|Word32|], "offset")
  , ([t|CSize|], "size")
  ]

genStruct "VkSpecializationInfo"
  [ ([t|Word32|], "mapEntryCount")
  , ([t|Ptr VkSpecializationMapEntry|], "pMapEntries")
  , ([t|CSize|], "dataSize")
  , ([t|Ptr ()|], "pData")
  ]

genStruct "VkPipelineShaderStageCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineShaderStageCreateFlags|], "flags")
  , ([t|VkShaderStageFlagBits|], "stage")
  , ([t|VkShaderModule|], "module")
  , ([t|Ptr CChar|], "pName")
  , ([t|Ptr VkSpecializationInfo|], "pSpecializationInfo")
  ]

type VkPipelineVertexInputStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkVertexInputRate"
  [ ("VK_VERTEX_INPUT_RATE_VERTEX", 0)
  , ("VK_VERTEX_INPUT_RATE_INSTANCE", 1)
  ]

genStruct "VkVertexInputBindingDescription"
  [ ([t|Word32|], "binding")
  , ([t|Word32|], "stride")
  , ([t|VkVertexInputRate|], "inputRate")
  ]

genStruct "VkVertexInputAttributeDescription"
  [ ([t|Word32|], "location")
  , ([t|Word32|], "binding")
  , ([t|VkFormat|], "format")
  , ([t|Word32|], "offset")
  ]

genStruct "VkPipelineVertexInputStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineVertexInputStateCreateFlags|], "flags")
  , ([t|Word32|], "vertexBindingDescriptionCount")
  , ([t|Ptr VkVertexInputBindingDescription|], "pVertexBindingDescriptions")
  , ([t|Word32|], "vertexAttributeDescriptionCount")
  , ([t|Ptr VkVertexInputAttributeDescription|], "pVertexAttributeDescriptions")
  ]

type VkPipelineInputAssemblyStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkPrimitiveTopology"
  [ ("VK_PRIMITIVE_TOPOLOGY_POINT_LIST", 0)
  , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST", 1)
  , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP", 2)
  , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST", 3)
  , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP", 4)
  , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN", 5)
  , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY", 6)
  , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY", 7)
  , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY", 8)
  , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY", 9)
  , ("VK_PRIMITIVE_TOPOLOGY_PATCH_LIST", 10)
  ]

genStruct "VkPipelineInputAssemblyStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineInputAssemblyStateCreateFlags|], "flags")
  , ([t|VkPrimitiveTopology|], "topology")
  , ([t|VkBool32|], "primitiveRestartEnable")
  ]

type VkPipelineTessellationStateCreateFlags = VkFlags

genStruct "VkPipelineTessellationStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineTessellationStateCreateFlags|], "flags")
  , ([t|Word32|], "patchControlPoints")
  ]

type VkPipelineViewportStateCreateFlags = VkFlags

genStruct "VkViewport"
  [ ([t|Float|], "x")
  , ([t|Float|], "y")
  , ([t|Float|], "width")
  , ([t|Float|], "height")
  , ([t|Float|], "minDepth")
  , ([t|Float|], "maxDepth")
  ]

genStruct "VkPipelineViewportStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineViewportStateCreateFlags|], "flags")
  , ([t|Word32|], "viewportCount")
  , ([t|Ptr VkViewport|], "pViewports")
  , ([t|Word32|], "scissorCount")
  , ([t|Ptr VkRect2D|], "pScissors")
  ]

type VkPipelineRasterizationStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkPolygonMode"
  [ ("VK_POLYGON_MODE_FILL", 0)
  , ("VK_POLYGON_MODE_LINE", 1)
  , ("VK_POLYGON_MODE_POINT", 2)
  ]

type VkCullModeFlags = VkFlags
genEnum [t|Word32|] "VkCullModeFlagBits"
  [ ("VK_CULL_MODE_NONE", 0)
  , ("VK_CULL_MODE_FRONT_BIT", 0x00000001)
  , ("VK_CULL_MODE_BACK_BIT", 0x00000002)
  , ("VK_CULL_MODE_FRONT_AND_BACK", 0x00000003)
  ]

genEnum [t|Word32|] "VkFrontFace"
  [ ("VK_FRONT_FACE_COUNTER_CLOCKWISE", 0)
  , ("VK_FRONT_FACE_CLOCKWISE", 1)
  ]

genStruct "VkPipelineRasterizationStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineRasterizationStateCreateFlags|], "flags")
  , ([t|VkBool32|], "depthClampEnable")
  , ([t|VkBool32|], "rasterizerDiscardEnable")
  , ([t|VkPolygonMode|], "polygonMode")
  , ([t|VkCullModeFlags|], "cullMode")
  , ([t|VkFrontFace|], "frontFace")
  , ([t|VkBool32|], "depthBiasEnable")
  , ([t|Float|], "depthBiasConstantFactor")
  , ([t|Float|], "depthBiasClamp")
  , ([t|Float|], "depthBiasSlopeFactor")
  , ([t|Float|], "lineWidth")
  ]

type VkPipelineMultisampleStateCreateFlags = VkFlags

genStruct "VkPipelineMultisampleStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineMultisampleStateCreateFlags|], "flags")
  , ([t|VkSampleCountFlagBits|], "rasterizationSamples")
  , ([t|VkBool32|], "sampleShadingEnable")
  , ([t|Float|], "minSampleShading")
  , ([t|Ptr VkSampleMask|], "pSampleMask")
  , ([t|VkBool32|], "alphaToCoverageEnable")
  , ([t|VkBool32|], "alphaToOneEnable")
  ]

type VkPipelineDepthStencilStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkCompareOp"
  [ ("VK_COMPARE_OP_NEVER", 0)
  , ("VK_COMPARE_OP_LESS", 1)
  , ("VK_COMPARE_OP_EQUAL", 2)
  , ("VK_COMPARE_OP_LESS_OR_EQUAL", 3)
  , ("VK_COMPARE_OP_GREATER", 4)
  , ("VK_COMPARE_OP_NOT_EQUAL", 5)
  , ("VK_COMPARE_OP_GREATER_OR_EQUAL", 6)
  , ("VK_COMPARE_OP_ALWAYS", 7)
  ]

genEnum [t|Word32|] "VkStencilOp"
  [ ("VK_STENCIL_OP_KEEP", 0)
  , ("VK_STENCIL_OP_ZERO", 1)
  , ("VK_STENCIL_OP_REPLACE", 2)
  , ("VK_STENCIL_OP_INCREMENT_AND_CLAMP", 3)
  , ("VK_STENCIL_OP_DECREMENT_AND_CLAMP", 4)
  , ("VK_STENCIL_OP_INVERT", 5)
  , ("VK_STENCIL_OP_INCREMENT_AND_WRAP", 6)
  , ("VK_STENCIL_OP_DECREMENT_AND_WRAP", 7)
  ]

genStruct "VkStencilOpState"
  [ ([t|VkStencilOp|], "failOp")
  , ([t|VkStencilOp|], "passOp")
  , ([t|VkStencilOp|], "depthFailOp")
  , ([t|VkCompareOp|], "compareOp")
  , ([t|Word32|], "compareMask")
  , ([t|Word32|], "writeMask")
  , ([t|Word32|], "reference")
  ]

genStruct "VkPipelineDepthStencilStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineDepthStencilStateCreateFlags|], "flags")
  , ([t|VkBool32|], "depthTestEnable")
  , ([t|VkBool32|], "depthWriteEnable")
  , ([t|VkCompareOp|], "depthCompareOp")
  , ([t|VkBool32|], "depthBoundsTestEnable")
  , ([t|VkBool32|], "stencilTestEnable")
  , ([t|VkStencilOpState|], "front")
  , ([t|VkStencilOpState|], "back")
  , ([t|Float|], "minDepthBounds")
  , ([t|Float|], "maxDepthBounds")
  ]

type VkPipelineColorBlendStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkBlendFactor"
  [ ("VK_BLEND_FACTOR_ZERO", 0)
  , ("VK_BLEND_FACTOR_ONE", 1)
  , ("VK_BLEND_FACTOR_SRC_COLOR", 2)
  , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR", 3)
  , ("VK_BLEND_FACTOR_DST_COLOR", 4)
  , ("VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR", 5)
  , ("VK_BLEND_FACTOR_SRC_ALPHA", 6)
  , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA", 7)
  , ("VK_BLEND_FACTOR_DST_ALPHA", 8)
  , ("VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA", 9)
  , ("VK_BLEND_FACTOR_CONSTANT_COLOR", 10)
  , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR", 11)
  , ("VK_BLEND_FACTOR_CONSTANT_ALPHA", 12)
  , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA", 13)
  , ("VK_BLEND_FACTOR_SRC_ALPHA_SATURATE", 14)
  , ("VK_BLEND_FACTOR_SRC1_COLOR", 15)
  , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR", 16)
  , ("VK_BLEND_FACTOR_SRC1_ALPHA", 17)
  , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA", 18)
  ]

genEnum [t|Word32|] "VkBlendOp"
  [ ("VK_BLEND_OP_ADD", 0)
  , ("VK_BLEND_OP_SUBTRACT", 1)
  , ("VK_BLEND_OP_REVERSE_SUBTRACT", 2)
  , ("VK_BLEND_OP_MIN", 3)
  , ("VK_BLEND_OP_MAX", 4)
  ]

type VkColorComponentFlags = VkFlags
genEnum [t|Word32|] "VkColorComponentFlagBits"
  [ ("VK_COLOR_COMPONENT_R_BIT", 0x00000001)
  , ("VK_COLOR_COMPONENT_G_BIT", 0x00000002)
  , ("VK_COLOR_COMPONENT_B_BIT", 0x00000004)
  , ("VK_COLOR_COMPONENT_A_BIT", 0x00000008)
  ]

genEnum [t|Word32|] "VkLogicOp"
  [ ("VK_LOGIC_OP_CLEAR", 0)
  , ("VK_LOGIC_OP_AND", 1)
  , ("VK_LOGIC_OP_AND_REVERSE", 2)
  , ("VK_LOGIC_OP_COPY", 3)
  , ("VK_LOGIC_OP_AND_INVERTED", 4)
  , ("VK_LOGIC_OP_NO_OP", 5)
  , ("VK_LOGIC_OP_XOR", 6)
  , ("VK_LOGIC_OP_OR", 7)
  , ("VK_LOGIC_OP_NOR", 8)
  , ("VK_LOGIC_OP_EQUIVALENT", 9)
  , ("VK_LOGIC_OP_INVERT", 10)
  , ("VK_LOGIC_OP_OR_REVERSE", 11)
  , ("VK_LOGIC_OP_COPY_INVERTED", 12)
  , ("VK_LOGIC_OP_OR_INVERTED", 13)
  , ("VK_LOGIC_OP_NAND", 14)
  , ("VK_LOGIC_OP_SET", 15)
  ]

genStruct "VkPipelineColorBlendAttachmentState"
  [ ([t|VkBool32|], "blendEnable")
  , ([t|VkBlendFactor|], "srcColorBlendFactor")
  , ([t|VkBlendFactor|], "dstColorBlendFactor")
  , ([t|VkBlendOp|], "colorBlendOp")
  , ([t|VkBlendFactor|], "srcAlphaBlendFactor")
  , ([t|VkBlendFactor|], "dstAlphaBlendFactor")
  , ([t|VkBlendOp|], "alphaBlendOp")
  , ([t|VkColorComponentFlags|], "colorWriteMask")
  ]

genStructWithArrays "VkPipelineColorBlendStateCreateInfo"
  [ ([t|VkStructureType|], "sType", 0)
  , ([t|Ptr ()|], "pNext", 0)
  , ([t|VkPipelineColorBlendStateCreateFlags|], "flags", 0)
  , ([t|VkBool32|], "logicOpEnable", 0)
  , ([t|VkLogicOp|], "logicOp", 0)
  , ([t|Word32|], "attachmentCount", 0)
  , ([t|Ptr VkPipelineColorBlendAttachmentState|], "pAttachments", 0)
  , ([t|Float|], "blendConstants", 4)
  ]

type VkPipelineDynamicStateCreateFlags = VkFlags

genEnum [t|Word32|] "VkDynamicState"
  [ ("VK_DYNAMIC_STATE_VIEWPORT", 0)
  , ("VK_DYNAMIC_STATE_SCISSOR", 1)
  , ("VK_DYNAMIC_STATE_LINE_WIDTH", 2)
  , ("VK_DYNAMIC_STATE_DEPTH_BIAS", 3)
  , ("VK_DYNAMIC_STATE_BLEND_CONSTANTS", 4)
  , ("VK_DYNAMIC_STATE_DEPTH_BOUNDS", 5)
  , ("VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK", 6)
  , ("VK_DYNAMIC_STATE_STENCIL_WRITE_MASK", 7)
  , ("VK_DYNAMIC_STATE_STENCIL_REFERENCE", 8)
  , ("VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV", 1000087000)
  , ("VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT", 1000099000)
  ]

genStruct "VkPipelineDynamicStateCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineDynamicStateCreateFlags|], "flags")
  , ([t|Word32|], "dynamicStateCount")
  , ([t|Ptr VkDynamicState|], "pDynamicStates")
  ]

genStruct "VkGraphicsPipelineCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineCreateFlags|], "flags")
  , ([t|Word32|], "stageCount")
  , ([t|Ptr VkPipelineShaderStageCreateInfo|], "pStages")
  , ([t|Ptr VkPipelineVertexInputStateCreateInfo|], "pVertexInputState")
  , ([t|Ptr VkPipelineInputAssemblyStateCreateInfo|], "pInputAssemblyState")
  , ([t|Ptr VkPipelineTessellationStateCreateInfo|], "pTessellationState")
  , ([t|Ptr VkPipelineViewportStateCreateInfo|], "pViewportState")
  , ([t|Ptr VkPipelineRasterizationStateCreateInfo|], "pRasterizationState")
  , ([t|Ptr VkPipelineMultisampleStateCreateInfo|], "pMultisampleState")
  , ([t|Ptr VkPipelineDepthStencilStateCreateInfo|], "pDepthStencilState")
  , ([t|Ptr VkPipelineColorBlendStateCreateInfo|], "pColorBlendState")
  , ([t|Ptr VkPipelineDynamicStateCreateInfo|], "pDynamicState")
  , ([t|VkPipelineLayout|], "layout")
  , ([t|VkRenderPass|], "renderPass")
  , ([t|Word32|], "subpass")
  , ([t|VkPipeline|], "basePipelineHandle")
  , ([t|Int32|], "basePipelineIndex")
  ]

genStruct "VkComputePipelineCreateInfo"
  [ ([t|VkStructureType|], "sType")
  , ([t|Ptr ()|], "pNext")
  , ([t|VkPipelineCreateFlags|], "flags")
  , ([t|VkPipelineShaderStageCreateInfo|], "stage")
  , ([t|VkPipelineLayout|], "layout")
  , ([t|VkPipeline|], "basePipelineHandle")
  , ([t|Int32|], "basePipelineIndex")
  ]

type FN_vkCreateGraphicsPipelines
  =  VkDevice
  -> VkPipelineCache
  -> Word32
  -> Ptr VkGraphicsPipelineCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkPipeline
  -> IO Word32 -- VkResult

type FN_vkCreateComputePipelines
  =  VkDevice
  -> VkPipelineCache
  -> Word32
  -> Ptr VkComputePipelineCreateInfo
  -> Ptr VkAllocationCallbacks
  -> Ptr VkPipeline
  -> IO Word32 -- VkResult

type FN_vkDestroyPipeline
  =  VkDevice
  -> VkPipeline
  -> Ptr VkAllocationCallbacks
  -> IO ()
