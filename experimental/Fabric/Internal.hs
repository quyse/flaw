{-|
Module: Flaw.Fabric.Internal
Description: Internal methods for serializing objects into graph.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.Fabric.Internal
	( Hash(..)
	, hash
	, hashBinary
	, HashedFabric(..)
	, Fabricable(..)
	, genPrimFabricable
	, genFabricable
	) where

import Language.Haskell.TH
import Data.List
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64BS
import qualified Data.ByteString.Char8 as C8BS
import qualified Crypto.Hash.SHA1 as SHA1

-- | Hash of object.
newtype Hash = Hash BS.ByteString deriving (Eq, Ord)

instance Binary Hash where
	put (Hash h) = put h
	get = get >>= return . Hash

instance Show Hash where
	showsPrec _ h = \s -> show h ++ s
	show (Hash h) = C8BS.unpack $ B64BS.encode h

hash :: BL.ByteString -> Hash
hash = Hash . SHA1.hashlazy

hashBinary :: Binary a => a -> Hash
hashBinary = hash . encode

-- | Object with its hash.
data HashedFabric a = HashedFabric Hash (Fabric a)


-- | Class of serializable objects.
class Fabricable a where
	data Fabric a :: *
	-- | Pack object into hashed object.
	fabric :: a -> HashedFabric a
	-- | Lazy unpack object from fabric.
	--unfabric :: HashedFabric a -> a

-- | hash binary function.
hashBinaryE :: Exp
hashBinaryE = VarE 'hashBinary
-- | HashedFabric constructor.
hashedFabricE :: Exp
hashedFabricE = ConE 'HashedFabric

-- | Generate Fabricable instance for a given primitive type.
-- Type should instance Binary class.
genPrimFabricable :: Name -> Q [Dec]
genPrimFabricable name = do
	let originalType = ConT name
	-- declaration of Fabric data instance
	fName <- newName $ nameBase name ++ "_Fabric"
	let fabricDataDec = DataInstD [] ''Fabric [originalType] [NormalC fName [(NotStrict, originalType)]] []
	-- declaration of fabric method
	fabricDec <- do
		-- method param
		paramName <- newName "a"
		let hashE = AppE hashBinaryE $ VarE paramName
		let body = NormalB $ AppE (AppE hashedFabricE hashE) $ AppE (ConE fName) $ VarE paramName
		return $ FunD 'fabric [Clause [VarP paramName] body []]
	-- unite all declarations
	let decs = [fabricDataDec, fabricDec]
	-- return instance
	return [InstanceD [] (AppT (ConT ''Fabricable) originalType) decs]

-- | Fabric constructor.
data FabricCon = FabricCon
	{ fabricConName :: Name
	, fabricConOriginalPat :: Pat
	, fabricConFabricExp :: Exp
	, fabricConFabricCon :: Con
	}

-- | Helper to create type HashedFabric t.
hbType :: Type -> Type
hbType t = AppT (ConT ''HashedFabric) t
-- | Helper to propagate HashedFabric into StrictType.
hbStrictType :: (Strict, Type) -> (Strict, Type)
hbStrictType (s, t) = (s, hbType t)

-- | Get name of TyVarBndr
getTvbName :: TyVarBndr -> Name
getTvbName tvb = case tvb of
	PlainTV name -> name
	KindedTV name _ -> name

-- | Get name and temp param name for VarStrictType
getVstName :: (Name, Strict, Type) -> Name
getVstName (fieldName, _, _) = fieldName

-- | Generate new name for fabricy things.
fabricName :: Name -> Q Name
fabricName name = newName $ nameBase name ++ "_Fabric"

-- | Transform original constructor into fabric constructor.
fabricCon :: Con -> Q FabricCon
fabricCon con = do
	let dec (p, bp, hp) = ValD (AsP bp $ ConP 'HashedFabric [VarP hp, WildP]) (NormalB $ AppE (VarE 'fabric) $ VarE p) []
	case con of
		NormalC name sts -> do
			fName <- fabricName name
			params <- mapM (\_ -> newName "p") sts
			fabricParams <- mapM (\_ -> newName "b") params
			hashParams <- mapM (\_ -> newName "h") params
			let decs = map dec $ zip3 params fabricParams hashParams
			let hashE = AppE hashBinaryE $ TupE $ (LitE $ StringL $ nameBase name) : (map VarE hashParams)
			return FabricCon
				{ fabricConName = fName
				, fabricConOriginalPat = ConP name $ map VarP params
				, fabricConFabricExp = LetE decs $ AppE (AppE hashedFabricE hashE) $ foldl AppE (ConE fName) $ map VarE fabricParams
				, fabricConFabricCon = NormalC fName $ map hbStrictType sts
				}
		RecC name vsts -> do
			fName <- fabricName name
			-- get sorted list of (fieldName, strict, type)
			let sortedVsts = sortBy (\(n1, s1, t1) (n2, s2, t2) -> compare n1 n2) vsts
			let fields = map getVstName sortedVsts
			params <- mapM (\_ -> newName "p") fields
			fabricParams <- mapM (\_ -> newName "b") fields
			hashParams <- mapM (\_ -> newName "h") fields
			let decs = map dec $ zip3 params fabricParams hashParams
			let hashE = AppE hashBinaryE $ TupE $ (LitE $ StringL $ nameBase name) : [TupE [LitE $ StringL $ nameBase f, VarE hp] | (f, hp) <- zip fields hashParams]
			fabricFields <- mapM (\fn -> newName $ nameBase fn ++ "_Fabric") fields
			return FabricCon
				{ fabricConName = fName
				, fabricConOriginalPat = RecP name $ zip fields $ map VarP params
				, fabricConFabricExp = LetE decs $ AppE (AppE hashedFabricE hashE) $ RecConE fName $ zip fabricFields $ map VarE fabricParams
				, fabricConFabricCon = RecC fName [(bn, s, hbType t) | (bn, (n, s, t)) <- zip fabricFields sortedVsts]
				}
		InfixC st1 name st2 -> do
			fName <- fabricName name
			p1 <- newName "p1"
			p2 <- newName "p2"
			bp1 <- newName "b1"
			bp2 <- newName "b2"
			hp1 <- newName "h1"
			hp2 <- newName "h2"
			let decs = map dec [(p1, bp1, hp1), (p2, bp2, hp2)]
			let hashE = AppE hashBinaryE $ TupE [LitE $ StringL $ nameBase name, VarE hp1, VarE hp2]
			return FabricCon
				{ fabricConName = fName
				, fabricConOriginalPat = InfixP (VarP p1) name (VarP p2)
				, fabricConFabricExp = LetE decs $ AppE (AppE hashedFabricE hashE) $ InfixE (Just $ VarE bp1) (VarE fName) (Just $ VarE bp2)
				, fabricConFabricCon = InfixC (hbStrictType st1) fName (hbStrictType st2)
				}
		ForallC bndrs cxt c -> do
			bc <- fabricCon c
			return bc
				{ fabricConFabricCon = ForallC bndrs cxt (fabricConFabricCon bc)
				}

-- | Generate Fabricable instance for a given type.
genFabricable :: Name -> Q [Dec]
genFabricable name = reify name >>= genFabricableFromInfo where
	-- | Generate Fabricable instance for given type info.
	genFabricableFromInfo (TyConI (DataD cxt name tvbs cons derivings)) = do
		-- construct original type
		let originalType = foldl AppT (ConT name) $ map (VarT . getTvbName) tvbs
		-- transform constructors
		fabricCons <- mapM fabricCon cons
		-- declaration of Fabric data instance
		let fabricDataDec = DataInstD [] ''Fabric [originalType] (map fabricConFabricCon fabricCons) []
		-- declaration of fabric method
		fabricDec <- do
			-- method param
			paramName <- newName "a"
			let match bc = do
				let matchBody = NormalB $ fabricConFabricExp bc
				return $ Match (fabricConOriginalPat bc) matchBody []
			matches <- mapM match fabricCons
			return $ FunD 'fabric [Clause [VarP paramName] (NormalB $ CaseE (VarE paramName) matches) []]
		-- unite all declarations
		let decs = [fabricDataDec, fabricDec]
		-- return instance
		return [InstanceD cxt (AppT (ConT ''Fabricable) originalType) decs]
