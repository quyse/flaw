{-|
Module: Flaw.Editor.Entity.Internal
Description: Entity internals.
License: MIT
-}

module Flaw.Editor.Entity.Internal
	( hashTextDecl
	) where

import qualified Crypto.Hash as C
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import Flaw.Build

-- | Calculate hash of text and store it in a top-level declaration.
hashTextDecl :: String -> TypeQ -> (ExpQ -> ExpQ) -> T.Text -> ExpQ
hashTextDecl declPrefix declType f str = do
	-- add hash as top decl
	cnt <- runIO $ atomicModifyIORef TH.counter $ \c -> (c + 1, c)
	n <- newName $ declPrefix <> show cnt
	TH.addTopDecls =<< sequence
		[ pragInlD n NoInline FunLike AllPhases
		, sigD n declType
		, valD (varP n) (normalB $ f $ embedExp (BA.convert (C.hash (T.encodeUtf8 str) :: C.Digest C.SHA1) :: B.ByteString)) []
		]
	varE n
