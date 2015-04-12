{-|
Module: Flaw.Build
Description: Basic definitions for build.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Build
	( loadFile
	, Embed(..)
	, fileExp
	, packList
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(qAddDependentFile)

-- | Load file and add it as a dependency.
loadFile :: FilePath -> Q BL.ByteString
loadFile filePath = do
	fileData <- runIO $ BL.readFile filePath
	qAddDependentFile filePath
	return fileData

-- | Class of data may be embedded.
class Embed a where
	-- | Construct an expression for the data (of type 'IO a').
	embedExp :: a -> Q Exp

instance Embed Int where
	embedExp n = [| return $(litE $ integerL $ fromIntegral n) |]

instance Embed Bool where
	embedExp b = [| return $(if b then (conE 'True) else (conE 'False)) |]

instance Embed B.ByteString where
	embedExp bytes = do
		[| B.unsafePackAddressLen $(litE $ integerL $ fromIntegral $ B.length bytes) $(litE $ stringPrimL $ B.unpack bytes) |]

instance Embed BL.ByteString where
	embedExp bytes = do
		[| B.unsafePackAddressLen $(litE $ integerL $ fromIntegral $ BL.length bytes) $(litE $ stringPrimL $ BL.unpack bytes) |]

instance Embed a => Embed [a] where
	embedExp a = [| sequence $(listE $ map embedExp a) |]

instance (Embed a, Embed b) => Embed (a, b) where
	embedExp (a, b) = [| do
		av <- $(embedExp a)
		bv <- $(embedExp b)
		return (av, bv)
		|]

instance (Embed a, Embed b, Embed c) => Embed (a, b, c) where
	embedExp (a, b, c) = [| do
		av <- $(embedExp a)
		bv <- $(embedExp b)
		cv <- $(embedExp c)
		return (av, bv, cv)
		|]

instance (Embed a, Embed b, Embed c, Embed d) => Embed (a, b, c, d) where
	embedExp (a, b, c, d) = [| do
		av <- $(embedExp a)
		bv <- $(embedExp b)
		cv <- $(embedExp c)
		dv <- $(embedExp d)
		return (av, bv, cv, dv)
		|]

-- | Embed file data as an expression.
fileExp :: FilePath -> Q Exp
fileExp filePath = do
	embedExp =<< loadFile filePath

-- | Pack storable list to bytestring.
packList :: Storable a => [a] -> IO B.ByteString
packList vs = do
	let len = length vs
	bytesPtr <- mallocArray len
	pokeArray bytesPtr vs
	B.unsafePackMallocCStringLen (castPtr bytesPtr, len * sizeOf (head vs))
