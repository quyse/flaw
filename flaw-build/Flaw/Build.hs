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
	, unpackVector
	, embedCStringExp
	, genEmbed
	) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified GHC.Ptr as GHC
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(addDependentFile)
import System.IO.Unsafe

-- | Load file and add it as a dependency.
loadFile :: FilePath -> Q BL.ByteString
loadFile filePath = do
	fileData <- runIO $ BL.readFile filePath
	addDependentFile filePath
	return fileData

-- | Class of pure data may be embedded.
-- An embedded expression is of type a.
class Embed a where
	embedExp :: a -> Q Exp

instance Embed Int where
	embedExp n = litE $ integerL $ fromIntegral n

instance Embed Float where
	embedExp n = litE $ rationalL $ toRational n

instance Embed Double where
	embedExp n = litE $ rationalL $ toRational n

instance Embed Bool where
	embedExp b = conE $ if b then 'True else 'False

instance Embed T.Text where
	embedExp t = [| T.pack $(litE $ stringL $ T.unpack t) |]

instance Embed Name where
	embedExp n = [| mkName $(litE $ stringL $ maybe "" (++ ".") (nameModule n) ++ nameBase n) |]

instance Embed B.ByteString where
	embedExp bytes = [| unsafePerformIO $(embedIOExp bytes) |]

instance Embed BL.ByteString where
	embedExp bytes = [| unsafePerformIO $(embedIOExp bytes) |]

instance Embed a => Embed (V.Vector a) where
	embedExp v = [| V.fromList $(embedExp $ V.toList v) |]

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
		let len = B.length bytes
		[| B.unsafePackAddressLen len $(litE $ stringPrimL $ B.unpack bytes) |]

instance EmbedIO BL.ByteString where
	embedIOExp bytes = do
		let len = BL.length bytes
		[| BL.fromStrict <$> B.unsafePackAddressLen len $(litE $ stringPrimL $ BL.unpack bytes) |]

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
fileExp filePath = embedIOExp =<< loadFile filePath

-- | Pack storable list to bytestring.
packList :: Storable a => [a] -> IO B.ByteString
packList vs = do
	let len = length vs
	bytesPtr <- mallocArray len
	pokeArray bytesPtr vs
	B.unsafePackMallocCStringLen (castPtr bytesPtr, len * sizeOf (head vs))

-- | Pack storable vector to bytestring.
packVector :: (Storable a, VG.Vector v a) => v a -> IO B.ByteString
packVector v = do
	let len = VG.length v
	bytesPtr <- mallocArray len
	VS.unsafeWith (VG.convert v) $ \vecPtr -> do
		copyArray bytesPtr vecPtr len
	B.unsafePackMallocCStringLen (castPtr bytesPtr, len * sizeOf (VG.head v))

-- | Unpack storable vector from bytestring.
unpackVector :: (Storable a, VG.Vector v a) => B.ByteString -> IO (v a)
unpackVector bytes = wu $ \u -> B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> VG.generateM (len `quot` sizeOf u) $ peekElemOff $ castPtr ptr
	where
		wu :: (a -> IO (v a)) -> IO (v a)
		wu m = m undefined

-- | Null-terminated string literal (of type Ptr CChar).
embedCStringExp :: String -> Q Exp
embedCStringExp str = [| GHC.Ptr $(litE $ stringPrimL $ B.unpack (T.encodeUtf8 $ T.pack str) ++ [0]) :: Ptr CChar |]

-- | Generate Embed instance for ADT.
{- Example:

data A a => T a
	= T1 a
	| T2 { f1 :: a, f2 :: a }
	| a :* a

genEmbed ''T:

instance A a => Embed (T a) where
	embedExp x = case x of
		T1 x1 -> appE (conE (mkName "T1")) (embedExp x1)
		T2 { f1 = x1, f2 = x2 } -> do
			e1 <- embedExp x1
			e2 <- embedExp x2
			recConE (mkName "T2")
				[ return (mkName "f1", e1)
				, return (mkName "f2", e2)
				]
		x1 :* x2 -> uInfixE (embedExp x1) (varE (mkName ":*")) (embedExp x2)

-}
genEmbed :: Name -> Q [Dec]
genEmbed dn = do
	info <- reify dn
	case info of
		TyConI (DataD dataContext dataName tvbs cons _derivings) -> process dataContext dataName tvbs cons
		TyConI (NewtypeD dataContext dataName tvbs con _derivings) -> process dataContext dataName tvbs [con]
		_ -> fail $ show ("unsupported declaration for embedding", info)
	where
	process dataContext dataName tvbs cons = do
		let
			tvns = [case tvb of
				PlainTV n -> n
				KindedTV n _k -> n
				| tvb <- tvbs]
			embedMatch con = case con of
				NormalC conName sts -> do
					xs <- mapM (newName . snd) $ zip sts ["x" ++ show (n :: Int) | n <- [1..]]
					let body = normalB $ foldl
						(\a b -> [| appE $a $b |])
						[| conE $(embedExp conName) |]
						(map (\v -> [| embedExp $(varE v) |]) xs)
					match (conP conName $ map varP xs) body []
				RecC conName vsts -> do
					xs <- forM [n | (n, _s, _t) <- vsts] $ \n -> do
						xn <- newName $ nameBase n
						en <- newName $ "e" ++ nameBase n
						return (n, xn, en)
					let enStmt (_n, xn, en) = bindS (varP en) [| embedExp $(varE xn) |]
					let fExp (n, _xn, en) = [| return ($(embedExp n), $(varE en)) |]
					let body = normalB $ doE $ map enStmt xs ++
						[noBindS [| recConE $(embedExp conName) $(listE $ map fExp xs) |]]
					match (recP conName [return (n, VarP xn) | (n, xn, _en) <- xs]) body []
				InfixC _st1 conName _st2 -> do
					x1 <- newName "x1"
					x2 <- newName "x2"
					let body = normalB
						[| uInfixE (embedExp $(varE x1)) (conE $(embedExp conName)) (embedExp $(varE x2)) |]
					match (infixP (varP x1) conName (varP x2)) body []
				ForallC _tvbs _cxt c -> embedMatch c
		x <- newName "x"
		sequence
			[ instanceD (return dataContext) (appT (conT ''Embed) $ foldl (\a b -> appT a (varT b)) (conT dataName) tvns)
				[ funD 'embedExp
					[ clause [varP x] (normalB $ caseE (varE x) $ map embedMatch cons) []
					]
				]
			]
