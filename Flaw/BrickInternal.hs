{-|
Module: Flaw.BrickInternal
Description: Internal methods for serializing objects into graph.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.BrickInternal
	( Hash
	, HashedBrick(..)
	, Brickable(..)
	, genPrimBrickable
	, genBrickable
	) where

import Language.Haskell.TH
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash.Whirlpool as Whirlpool

-- | Hash of object.
newtype Hash = Hash BS.ByteString

hash :: BL.ByteString -> Hash
hash = Hash . Whirlpool.hashlazy

-- | TEST: empty hash
emptyHash :: Hash
emptyHash = Hash BS.empty

-- | Object with its hash.
data HashedBrick a = HashedBrick Hash (Brick a)

-- | Class of serializable objects.
class Brickable a where
	data Brick a :: *
	-- | Pack object into hashed object.
	brick :: a -> HashedBrick a
	-- | Lazy unpack object from brick.
	--unbrick :: HashedBrick a -> a

-- | brick function.
brickE :: Exp
brickE = VarE 'brick
-- | HashedBrick constructor.
hashedBrickE :: Exp
hashedBrickE = ConE 'HashedBrick
-- | Empty hash expression.
emptyHashE :: Exp
emptyHashE = VarE 'emptyHash

-- Instance for list.
instance Brickable a => Brickable [a] where
	data Brick [a] = List_Brick_Empty | List_Brick (HashedBrick a) (HashedBrick [a])
	brick a = case a of
		[] -> HashedBrick emptyHash List_Brick_Empty
		(head:tail) -> HashedBrick emptyHash $ List_Brick (brick head) (brick tail)

-- Instances for tuples.
instance (Brickable a, Brickable b) => Brickable (a, b) where
	data Brick (a, b) = Tuple2_Brick (HashedBrick a, HashedBrick b)
	brick (a, b) = HashedBrick emptyHash $ Tuple2_Brick (brick a, brick b)

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
		let hashE = AppE (VarE 'hash) $ AppE (VarE 'encode) $ VarE paramName
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
-- | Helper to propagate HashedBrick into VarStrictType.
hbVarStrictType :: (Name, Strict, Type) -> (Name, Strict, Type)
hbVarStrictType (n, s, t) = (n, s, hbType t)

-- | Get name of TyVarBndr
getTvbName :: TyVarBndr -> Name
getTvbName tvb = case tvb of
	PlainTV name -> name
	KindedTV name _ -> name

-- | Get name and temp param name for VarStrictType
tempVst :: (Name, Strict, Type) -> Q (Name, Name)
tempVst (fieldName, _, _) = do
	paramName <- newName $ nameBase fieldName
	return (fieldName, paramName)

-- | Generate new name for bricky things.
brickName :: Name -> Q Name
brickName name = newName $ nameBase name ++ "_Brick"

-- | Transform original constructor into brick constructor.
brickCon :: Con -> Q BrickCon
brickCon con = do
	case con of
		NormalC name sts -> do
			bName <- brickName name
			params <- mapM (\_ -> newName "p") sts
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = ConP name $ map VarP params
				, brickConBrickExp = foldl AppE (ConE bName) $ map ((AppE brickE) . VarE) params
				, brickConBrickCon = NormalC bName $ map hbStrictType sts
				}
		RecC name vsts -> do
			bName <- brickName name
			params <- mapM tempVst vsts
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = RecP name [(fieldName, VarP paramName) | (fieldName, paramName) <- params]
				, brickConBrickExp = RecConE name [(fieldName, AppE brickE $ VarE paramName) | (fieldName, paramName) <- params]
				, brickConBrickCon = RecC bName $ map hbVarStrictType vsts
				}
		InfixC st1 name st2 -> do
			bName <- brickName name
			p1 <- newName "p1"
			p2 <- newName "p2"
			return BrickCon
				{ brickConName = bName
				, brickConOriginalPat = InfixP (VarP p1) name (VarP p2)
				, brickConBrickExp = InfixE (Just $ AppE brickE $ VarE p1) (VarE bName) (Just $ AppE brickE $ VarE p2)
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
			let matches = [Match (brickConOriginalPat bc) (NormalB $ AppE (AppE hashedBrickE emptyHashE) $ brickConBrickExp bc) [] | bc <- brickCons]
			let clause = Clause [VarP paramName] (NormalB $ CaseE (VarE paramName) matches) []
			return $ FunD 'brick [clause]
		-- unite all declarations
		let decs = [brickDataDec, brickDec]
		-- return instance
		return [InstanceD cxt (AppT (ConT ''Brickable) originalType) decs]
