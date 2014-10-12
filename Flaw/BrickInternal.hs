{-|
Module: Flaw.BrickInternal
Description: Internal methods for serializing objects into graph.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.BrickInternal
	( Hash(..)
	, hash
	, hashBinary
	, HashedBrick(..)
	, Brickable(..)
	, genPrimBrickable
	, genBrickable
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
data HashedBrick a = HashedBrick Hash (Brick a)


-- | Class of serializable objects.
class Brickable a where
	data Brick a :: *
	-- | Pack object into hashed object.
	brick :: a -> HashedBrick a
	-- | Lazy unpack object from brick.
	--unbrick :: HashedBrick a -> a

-- | hash binary function.
hashBinaryE :: Exp
hashBinaryE = VarE 'hashBinary
-- | HashedBrick constructor.
hashedBrickE :: Exp
hashedBrickE = ConE 'HashedBrick

-- Instance for list.
instance Brickable a => Brickable [a] where
	data Brick [a] = List_Brick [HashedBrick a]
	brick xs = HashedBrick h $ List_Brick bs where
		bs = map brick xs
		hs = [h | HashedBrick h _ <- bs]
		h = hashBinary ("[]", hs)

-- Instances for tuples.
instance (Brickable a, Brickable b) => Brickable (a, b) where
	data Brick (a, b) = Tuple2_Brick (HashedBrick a) (HashedBrick b)
	brick (a, b) = HashedBrick h $ Tuple2_Brick ba bb where
		ba @ (HashedBrick ha _) = brick a
		bb @ (HashedBrick hb _) = brick b
		h = hashBinary ("(,)", ha, hb)

-- | Generate Brickable instance for a given primitive type.
-- Type should instance Binary class.
genPrimBrickable :: Name -> Q [Dec]
genPrimBrickable name = do
	let originalType = ConT name
	-- declaration of Brick data instance
	bName <- newName $ nameBase name ++ "_Brick"
	let brickDataDec = DataInstD [] ''Brick [originalType] [NormalC bName [(NotStrict, originalType)]] []
	-- declaration of brick method
	brickDec <- do
		-- method param
		paramName <- newName "a"
		let hashE = AppE hashBinaryE $ VarE paramName
		let body = NormalB $ AppE (AppE hashedBrickE hashE) $ AppE (ConE bName) $ VarE paramName
		let clause = Clause [VarP paramName] body []
		return $ FunD 'brick [clause]
	-- unite all declarations
	let decs = [brickDataDec, brickDec]
	-- return instance
	return [InstanceD [] (AppT (ConT ''Brickable) originalType) decs]

-- | Brick constructor.
data BrickCon = BrickCon
	{ brickConName :: Name
	, brickConOriginalPat :: Pat
	, brickConBrickExp :: Exp
	, brickConBrickCon :: Con
	}

-- | Helper to create type HashedBrick t.
hbType :: Type -> Type
hbType t = AppT (ConT ''HashedBrick) t
-- | Helper to propagate HashedBrick into StrictType.
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

-- | Generate new name for bricky things.
brickName :: Name -> Q Name
brickName name = newName $ nameBase name ++ "_Brick"

-- | Transform original constructor into brick constructor.
brickCon :: Con -> Q BrickCon
brickCon con = do
	let dec (p, bp, hp) = ValD (AsP bp $ ConP 'HashedBrick [VarP hp, WildP]) (NormalB $ AppE (VarE 'brick) $ VarE p) []
	case con of
		NormalC name sts -> do
			bName <- brickName name
			params <- mapM (\_ -> newName "p") sts
			brickParams <- mapM (\_ -> newName "b") params
			hashParams <- mapM (\_ -> newName "h") params
			let decs = map dec $ zip3 params brickParams hashParams
			let hashE = AppE hashBinaryE $ TupE $ (LitE $ StringL $ nameBase name) : (map VarE hashParams)
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = ConP name $ map VarP params
				, brickConBrickExp = LetE decs $ AppE (AppE hashedBrickE hashE) $ foldl AppE (ConE bName) $ map VarE brickParams
				, brickConBrickCon = NormalC bName $ map hbStrictType sts
				}
		RecC name vsts -> do
			bName <- brickName name
			-- get sorted list of (fieldName, strict, type)
			let sortedVsts = sortBy (\(n1, s1, t1) (n2, s2, t2) -> compare n1 n2) vsts
			let fields = map getVstName sortedVsts
			params <- mapM (\_ -> newName "p") fields
			brickParams <- mapM (\_ -> newName "b") fields
			hashParams <- mapM (\_ -> newName "h") fields
			let decs = map dec $ zip3 params brickParams hashParams
			let hashE = AppE hashBinaryE $ TupE $ (LitE $ StringL $ nameBase name) : [TupE [LitE $ StringL $ nameBase f, VarE hp] | (f, hp) <- zip fields hashParams]
			brickFields <- mapM (\fn -> newName $ nameBase fn ++ "_Brick") fields
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = RecP name $ zip fields $ map VarP params
				, brickConBrickExp = LetE decs $ AppE (AppE hashedBrickE hashE) $ RecConE bName $ zip brickFields $ map VarE brickParams
				, brickConBrickCon = RecC bName [(bn, s, hbType t) | (bn, (n, s, t)) <- zip brickFields sortedVsts]
				}
		InfixC st1 name st2 -> do
			bName <- brickName name
			p1 <- newName "p1"
			p2 <- newName "p2"
			bp1 <- newName "b1"
			bp2 <- newName "b2"
			hp1 <- newName "h1"
			hp2 <- newName "h2"
			let decs = map dec [(p1, bp1, hp1), (p2, bp2, hp2)]
			let hashE = AppE hashBinaryE $ TupE [LitE $ StringL $ nameBase name, VarE hp1, VarE hp2]
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = InfixP (VarP p1) name (VarP p2)
				, brickConBrickExp = LetE decs $ AppE (AppE hashedBrickE hashE) $ InfixE (Just $ VarE bp1) (VarE bName) (Just $ VarE bp2)
				, brickConBrickCon = InfixC (hbStrictType st1) bName (hbStrictType st2)
				}
		ForallC bndrs cxt c -> do
			bc <- brickCon c
			return bc
				{ brickConBrickCon = ForallC bndrs cxt (brickConBrickCon bc)
				}

-- | Generate Brickable instance for a given type.
genBrickable :: Name -> Q [Dec]
genBrickable name = reify name >>= genBrickableFromInfo where
	-- | Generate Brickable instance for given type info.
	genBrickableFromInfo (TyConI (DataD cxt name tvbs cons derivings)) = do
		-- construct original type
		let originalType = foldl AppT (ConT name) $ map (VarT . getTvbName) tvbs
		-- transform constructors
		brickCons <- mapM brickCon cons
		-- declaration of Brick data instance
		let brickDataDec = DataInstD [] ''Brick [originalType] (map brickConBrickCon brickCons) []
		-- declaration of brick method
		brickDec <- do
			-- method param
			paramName <- newName "a"
			let match bc = do
				let matchBody = NormalB $ brickConBrickExp bc
				return $ Match (brickConOriginalPat bc) matchBody []
			matches <- mapM match brickCons
			let clause = Clause [VarP paramName] (NormalB $ CaseE (VarE paramName) matches) []
			return $ FunD 'brick [clause]
		-- unite all declarations
		let decs = [brickDataDec, brickDec]
		-- return instance
		return [InstanceD cxt (AppT (ConT ''Brickable) originalType) decs]
