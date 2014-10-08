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
	brickConName <- newName $ nameBase name ++ "_Brick"
	let brickDataDec = DataInstD [] ''Brick [originalType] [NormalC brickConName [(NotStrict, originalType)]] []
	-- declaration of brick method
	brickDec <- do
		-- method param
		paramName <- newName "a"
		let hashE = AppE (VarE 'hash) $ AppE (VarE 'encode) $ VarE paramName
		let body = NormalB $ AppE (AppE hashedBrickE hashE) $ AppE (ConE brickConName) $ VarE paramName
		let clause = Clause [VarP paramName] body []
		return $ FunD 'brick [clause]
	-- unite all declarations
	let decs = [brickDataDec, brickDec]
	-- return instance
	return [InstanceD [] (AppT (ConT ''Brickable) originalType) decs]

-- | Generate Brickable instance for a given type.
genBrickable :: Name -> Q [Dec]
genBrickable name = reify name >>= genBrickableFromInfo where
	-- | Helper to create type HashedBrick t.
	hbType t = AppT (ConT ''HashedBrick) t
	-- | Helper to propagate HashedBrick into StrictType.
	hbStrictType (s, t) = (s, hbType t)
	-- | Helper to propagate HashedBrick into VarStrictType.
	hbVarStrictType (n, s, t) = (n, s, hbType t)
	-- | Get name of TyVarBndr
	getTvbName tvb = case tvb of
		PlainTV name -> name
		KindedTV name _ -> name
	-- | Generate new name for a constructor.
	newConName name = newName $ nameBase name ++ "_Brick"
	-- | Get name and temp param name for VarStrictType
	tempVst (fieldName, _, _) = do
		paramName <- newName $ nameBase fieldName
		return (fieldName, paramName)
	-- | Transform original constructor into brick constructor.
	-- Returns name, pattern for original constructor,
	-- expression for initializing brick,
	-- and Brick constructor.
	transformCon :: Con -> Q (Name, Pat, Exp, Con)
	transformCon con = do
		case con of
			NormalC name sts -> do
				bName <- newConName name
				params <- mapM (\_ -> newName "p") sts
				return
					( bName
					, ConP name $ map VarP params
					, foldl AppE (ConE bName) $ map ((AppE brickE) . VarE) params
					, NormalC bName $ map hbStrictType sts
					)
			RecC name vsts -> do
				bName <- newConName name
				params <- mapM tempVst vsts
				return
					( bName
					, RecP name [(fieldName, VarP paramName) | (fieldName, paramName) <- params]
					, RecConE name [(fieldName, AppE brickE $ VarE paramName) | (fieldName, paramName) <- params]
					, RecC bName $ map hbVarStrictType vsts
					)
			InfixC st1 name st2 -> do
				bName <- newConName name
				p1 <- newName "p1"
				p2 <- newName "p2"
				return
					( bName
					, InfixP (VarP p1) name (VarP p2)
					, InfixE (Just $ AppE brickE $ VarE p1) (VarE bName) (Just $ AppE brickE $ VarE p2)
					, InfixC (hbStrictType st1) bName (hbStrictType st2)
					)
			ForallC bndrs cxt c -> do
				(tn, tp, te, tc) <- transformCon c
				return
					( tn
					, tp
					, te
					, ForallC bndrs cxt tc
					)
	-- | Generate Brickable instance for given type info.
	genBrickableFromInfo (TyConI (DataD cxt name tvbs cons derivings)) = do
		-- construct original type
		let originalType = foldl AppT (ConT name) $ map (VarT . getTvbName) tvbs
		-- transform constructors
		brickCons <- mapM transformCon cons
		-- declaration of Brick data instance
		let brickDataDec = DataInstD [] ''Brick [originalType] [c | (n, p, e, c) <- brickCons] []
		-- declaration of brick method
		brickDec <- do
			-- method param
			paramName <- newName "a"
			let matches = [Match p (NormalB $ AppE (AppE hashedBrickE emptyHashE) e) [] | (n, p, e, c) <- brickCons]
			let clause = Clause [VarP paramName] (NormalB $ CaseE (VarE paramName) matches) []
			return $ FunD 'brick [clause]
		-- unite all declarations
		let decs = [brickDataDec, brickDec]
		-- return instance
		return [InstanceD cxt (AppT (ConT ''Brickable) originalType) decs]
