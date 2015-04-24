{-|
Module: Flaw.Build
Description: Basic definitions for build.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Build
	( loadFile
	, Embed(..)
	, EmbedIO(..)
	, embedStringExp
	, fileExp
	, packList
	, packVector
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
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

-- | Class of pure data may be embedded.
-- An embedded expression is of type a.
class Embed a where
	embedExp :: a -> Q Exp

instance Embed Int where
	embedExp n = litE $ integerL $ fromIntegral n

instance Embed Bool where
	embedExp b = if b then (conE 'True) else (conE 'False)

instance Embed T.Text where
	embedExp t = [| T.pack $(litE $ stringL $ T.unpack t) |]

instance Embed a => Embed [a] where
	embedExp a = listE $ map embedExp a

instance (Embed a, Embed b) => Embed (a, b) where
	embedExp (a, b) = [| ($(embedExp a), $(embedExp b)) |]

instance (Embed a, Embed b, Embed c) => Embed (a, b, c) where
	embedExp (a, b, c) = [| ($(embedExp a), $(embedExp b), $(embedExp c)) |]

instance (Embed a, Embed b, Embed c, Embed d) => Embed (a, b, c, d) where
	embedExp (a, b, c, d) = [| ($(embedExp a), $(embedExp b), $(embedExp c), $(embedExp d)) |]

-- | Class of data may be embedded.
-- An embedded expression is of type IO a.
class EmbedIO a where
	-- | Construct an expression for the data (of type 'IO a').
	embedIOExp :: a -> Q Exp

instance EmbedIO Int where
	embedIOExp n = [| return $(embedExp n) |]

instance EmbedIO Bool where
	embedIOExp b = [| return $(embedExp b) |]

instance EmbedIO B.ByteString where
	embedIOExp bytes = do
		[| B.unsafePackAddressLen $(litE $ integerL $ fromIntegral $ B.length bytes) $(litE $ stringPrimL $ B.unpack bytes) |]

instance EmbedIO BL.ByteString where
	embedIOExp bytes = do
		[| B.unsafePackAddressLen $(litE $ integerL $ fromIntegral $ BL.length bytes) $(litE $ stringPrimL $ BL.unpack bytes) |]

instance EmbedIO a => EmbedIO [a] where
	embedIOExp a = [| sequence $(listE $ map embedIOExp a) |]

instance (EmbedIO a, EmbedIO b) => EmbedIO (a, b) where
	embedIOExp (a, b) = [| do
		av <- $(embedIOExp a)
		bv <- $(embedIOExp b)
		return (av, bv)
		|]

instance (EmbedIO a, EmbedIO b, EmbedIO c) => EmbedIO (a, b, c) where
	embedIOExp (a, b, c) = [| do
		av <- $(embedIOExp a)
		bv <- $(embedIOExp b)
		cv <- $(embedIOExp c)
		return (av, bv, cv)
		|]

instance (EmbedIO a, EmbedIO b, EmbedIO c, EmbedIO d) => EmbedIO (a, b, c, d) where
	embedIOExp (a, b, c, d) = [| do
		av <- $(embedIOExp a)
		bv <- $(embedIOExp b)
		cv <- $(embedIOExp c)
		dv <- $(embedIOExp d)
		return (av, bv, cv, dv)
		|]

-- | Embed string as an expression.
embedStringExp :: String -> Q Exp
embedStringExp s = litE $ stringL s

-- | EmbedIO file data as an expression.
fileExp :: FilePath -> Q Exp
fileExp filePath = do
	embedIOExp =<< loadFile filePath

-- | Pack storable list to bytestring.
packList :: Storable a => [a] -> IO B.ByteString
packList vs = do
	let len = length vs
	bytesPtr <- mallocArray len
	pokeArray bytesPtr vs
	B.unsafePackMallocCStringLen (castPtr bytesPtr, len * sizeOf (head vs))

-- | Pack storable vector to bytestring.
packVector :: (Storable a, V.Vector v a) => v a -> IO B.ByteString
packVector v = do
	let len = V.length v
	bytesPtr <- mallocArray len
	VS.unsafeWith (V.convert v) $ \vecPtr -> do
		copyArray bytesPtr vecPtr len
	B.unsafePackMallocCStringLen (castPtr bytesPtr, len * sizeOf (V.head v))
