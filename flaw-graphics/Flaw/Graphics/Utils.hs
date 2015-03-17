{-|
Module: Flaw.Graphics.Utils
Description: Graphics helpers.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Graphics.Utils
	( UniformBuffer
	, UniformBufferBuilder
	, newUniformBuffer
	, addUniform
	, finalizeUniformBuffer
	, setUniform
	, renderUniform
	, uploadUniformBuffer
	, renderUploadUniformBuffer
	, renderUniformBuffer
	) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Storable

import Flaw.Graphics.Internal
import Flaw.Graphics.Program
import Flaw.Graphics.Program.Internal

data UniformBuffer d = UniformBuffer
	{ ubSlot :: Int
	, ubId :: UniformBufferId d
	, ubBytes :: ForeignPtr ()
	, ubSize :: Int
	}

data UniformBufferBuilder = UniformBufferBuilder
	{ ubbSlot :: Int
	, ubbSizeRef :: IORef Int
	}

newUniformBuffer :: Int -> IO UniformBufferBuilder
newUniformBuffer slot = do
	sizeRef <- newIORef 0
	return UniformBufferBuilder
		{ ubbSlot = slot
		, ubbSizeRef = sizeRef
		}

addUniform :: (OfValueType a, Storable a) => UniformBufferBuilder -> Int -> IO (Node a)
addUniform UniformBufferBuilder
	{ ubbSlot = slot
	, ubbSizeRef = sizeRef
	} size = withUndefined func where
	withUndefined :: (a -> IO (Node a)) -> IO (Node a)
	withUndefined f = f undefined
	func u = do
		bufferSize <- readIORef sizeRef
		let align = alignment u
		let alignedBufferSize = ((bufferSize + align - 1) `div` align) * align
		writeIORef sizeRef $ alignedBufferSize + (sizeOf u) * (if size > 0 then size else 1)
		return $ uniform slot alignedBufferSize size

finalizeUniformBuffer :: (MonadResource m, MonadBaseControl IO m, Device d) => d -> UniformBufferBuilder -> m (ReleaseKey, UniformBuffer d)
finalizeUniformBuffer device UniformBufferBuilder
	{ ubbSlot = slot
	, ubbSizeRef = sizeRef
	} = do
	size <- liftIO $ readIORef sizeRef
	-- align just in case
	let alignedSize = ((size + 15) `div` 16) * 16
	(releaseKey, uniformBufferId) <- createUniformBuffer device alignedSize
	bytes <- liftIO $ mallocForeignPtrBytes alignedSize
	return (releaseKey, UniformBuffer
		{ ubSlot = slot
		, ubId = uniformBufferId
		, ubBytes = bytes
		, ubSize = alignedSize
		})

setUniform :: (OfValueType a, Storable a) => UniformBuffer d -> Node a -> a -> IO ()
setUniform UniformBuffer
	{ ubBytes = bytes
	} (UniformNode Uniform
	{ uniformOffset = offset
	}) value = do
	withForeignPtr bytes $ \ptr -> do
		pokeByteOff ptr offset value
setUniform _ _ _ = undefined

renderUniform :: (OfValueType a, Storable a) => UniformBuffer d -> Node a -> a -> Render c d ()
renderUniform uniformBuffer node value = liftIO $ setUniform uniformBuffer node value

uploadUniformBuffer :: Context c d => c -> UniformBuffer d -> IO ()
uploadUniformBuffer context UniformBuffer
	{ ubId = uniformBufferId
	, ubBytes = bytes
	, ubSize = size
	} = do
	withForeignPtr bytes $ \ptr -> do
		contextUploadUniformBuffer context uniformBufferId ptr size

renderUploadUniformBuffer :: Context c d => UniformBuffer d -> Render c d ()
renderUploadUniformBuffer uniformBuffer = Render $ \context state -> do
	uploadUniformBuffer context uniformBuffer
	return (state, ())

renderUniformBuffer :: UniformBuffer d -> Render c d ()
renderUniformBuffer UniformBuffer
	{ ubSlot = slot
	, ubId = uniformBufferId
	} = renderDesire $ \s@RenderState
	{ renderStateUniformBuffers = uniformBuffers
	} -> s
	{ renderStateUniformBuffers = HashMap.insert slot uniformBufferId uniformBuffers
	}
