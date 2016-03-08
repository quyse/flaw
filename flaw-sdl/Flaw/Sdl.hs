{-|
Module: Flaw.Sdl
Description: SDL helper routines.
License: MIT
-}

module Flaw.Sdl
	( SdlException(..)
	, initSdlVideo
	, checkSdlError
	, checkSdlResult
	) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified SDL.Raw.Basic as SDL
import qualified SDL.Raw.Enum as SDL
import qualified SDL.Raw.Error as SDL
import System.IO.Unsafe(unsafePerformIO)

data SdlException = SdlException String deriving Show

instance Exception SdlException

-- | Initialized SDL subsystems flags, and number of initializations made.
flagsVar :: MVar (SDL.InitFlag, Int)
{-# NOINLINE flagsVar #-}
flagsVar = unsafePerformIO $ newMVar (0, 0)

deinitSdl :: IO ()
deinitSdl = do
	(flags, initCount) <- takeMVar flagsVar
	if initCount == 1 then do
		SDL.quitSubSystem flags
		SDL.quit
		putMVar flagsVar (0, 0)
	else do
		putMVar flagsVar (flags, initCount - 1)

initSdlSubsystem :: SDL.InitFlag -> IO ((), IO ())
initSdlSubsystem newFlags = do
	(flags, initCount) <- takeMVar flagsVar
	let additionalFlags = newFlags .&. complement flags
	when (additionalFlags > 0) $ do
		-- if SDL wasn't initialized before, do it
		when (flags == 0) $ checkSdlError (>= 0) $ SDL.init 0
		-- initialize subsystems
		checkSdlError (>= 0) $ SDL.initSubSystem additionalFlags
	putMVar flagsVar (flags .|. newFlags, initCount + 1)
	return ((), deinitSdl)

initSdlVideo :: IO ((), IO ())
initSdlVideo = initSdlSubsystem SDL.SDL_INIT_VIDEO

checkSdlError :: (CInt -> Bool) -> IO CInt -> IO ()
checkSdlError isSuccess f = do
	r <- f
	unless (isSuccess r) $ throwIO . SdlException =<< peekCString =<< SDL.getError

checkSdlResult :: IO (Ptr ()) -> IO (Ptr ())
checkSdlResult f = do
	r <- f
	if r == nullPtr then throwIO . SdlException =<< peekCString =<< SDL.getError
	else return r
