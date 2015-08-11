{-|
Module: Flaw.Sdl
Description: SDL helper routines.
License: MIT
-}

module Flaw.Sdl
	( SdlException(..)
	, checkSdlError
	, checkSdlResult
	) where

import Control.Exception
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.UI.SDL.Basic as SDL

data SdlException = SdlException String deriving Show

instance Exception SdlException

checkSdlError :: (CInt -> Bool) -> IO CInt -> IO ()
checkSdlError isSuccess f = do
	r <- f
	if isSuccess r then return ()
	else throwIO . SdlException =<< peekCString =<< SDL.getError

checkSdlResult :: IO (Ptr ()) -> IO (Ptr ())
checkSdlResult f = do
	r <- f
	if r == nullPtr then throwIO . SdlException =<< peekCString =<< SDL.getError
	else return r
