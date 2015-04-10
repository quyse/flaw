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
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
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

-- | Embed file data as an expression.
fileExp :: FilePath -> Q Exp
fileExp filePath = do
	embedExp =<< loadFile filePath
