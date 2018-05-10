{-|
Module: Flaw.Script.Lua.Operations
Description: Operations on Lua values.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.Script.Lua.Operations
	( luaCoerceToNumber
	, luaCoerceToInt
	, luaCoerceToBool
	, luaCoerceToString
	, luaValueShow
	, luaValueAdd
	, luaValueSub
	, luaValueMul
	, luaValueMod
	, luaValuePow
	, luaValueDiv
	, luaValueIDiv
	, luaValueBAnd
	, luaValueBOr
	, luaValueBXor
	, luaValueShl
	, luaValueShr
	, luaValueUnm
	, luaValueBNot
	, luaValueNot
	, luaValueLen
	, luaValueConcat
	, luaValueEq
	, luaValueLt
	, luaValueLe
	, luaValueGet
	, luaValueSet
	, luaValueCall
	, luaNewTable
	, luaNewTableSized
	, luaNewClosure
	) where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.HashTable.Class as HT(toList)
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Primitive.MutVar
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import Data.Unique
import Data.Word

import Flaw.Script.Lua

{-# INLINABLE luaCoerceToNumber #-}
luaCoerceToNumber :: LuaValue m -> Maybe Double
luaCoerceToNumber v = case v of
	LuaInteger i -> Just $ fromIntegral i
	LuaReal r -> Just r
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINABLE luaCoerceToInt #-}
luaCoerceToInt :: LuaValue m -> Maybe Int
luaCoerceToInt v = case v of
	LuaInteger i -> Just i
	LuaReal r -> let i = floor r in if r == fromIntegral i then Just i else Nothing
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINABLE luaCoerceToBool #-}
luaCoerceToBool :: LuaValue m -> Word8
luaCoerceToBool v = case v of
	LuaNil -> 0
	LuaBoolean b -> b
	_ -> 1

{-# INLINABLE luaCoerceToString #-}
luaCoerceToString :: LuaValue m -> Maybe T.Text
luaCoerceToString v = case v of
	LuaInteger i -> Just $ T.pack $ show i
	LuaReal r -> Just $ T.pack $ show r
	LuaString s -> Just s
	_ -> Nothing

{-# INLINABLE luaValueShow #-}
luaValueShow :: LuaMonad m => LuaValue m -> m TL.Builder
luaValueShow a = case a of
	LuaNil -> return "nil"
	LuaBoolean b -> return $ if b > 0 then "true" else "false"
	LuaInteger i -> return $ TL.fromString $ show i
	LuaReal r -> return $ TL.fromString $ show r
	LuaString t -> return $ TL.fromString $ show $ T.unpack t
	LuaClosure
		{ luaClosureUnique = u
		} -> return $ "<<closure" <> TL.fromString (show $ hashUnique u) <> ">>"
	LuaUserData
		{ luaUserDataUnique = u
		} -> return $ "<<userdata" <> TL.fromString (show $ hashUnique u) <> ">>"
	LuaTable
		{ luaTable = t
		} -> do
		l <- liftPrim $ HT.toList t
		s <- forM l $ \(k, v) -> do
			ks <- luaValueShow k
			vs <- luaValueShow v
			return $ "[ " <> ks <> " ] = " <> vs <> "; "
		return $ "{ " <> foldr (<>) "}" s

{-# INLINABLE getMetaTable #-}
getMetaTable :: LuaMonad m => LuaValue m -> m (Maybe (HT.HashTable (PrimState m) (LuaValue m) (LuaValue m)))
getMetaTable v = case v of
	LuaTable
		{ luaTableMetaTable = metaTableVar
		} -> do
		metaTable <- readMutVar metaTableVar
		return $ case metaTable of
			LuaTable
				{ luaTable = table
				} -> Just table
			_ -> Nothing
	_ -> return Nothing

{-# INLINABLE tryUnaryMetaMethod #-}
tryUnaryMetaMethod :: LuaMonad m => T.Text -> LuaValue m -> m (LuaValue m)
tryUnaryMetaMethod opName a = tryUnaryMetaMethodOr opName a $ throwLuaError $ LuaBadOperation opName

{-# INLINABLE tryUnaryMetaMethodOr #-}
tryUnaryMetaMethodOr :: LuaMonad m => T.Text -> LuaValue m -> m (LuaValue m) -> m (LuaValue m)
tryUnaryMetaMethodOr opName a other = do
	maybeMetaTable <- getMetaTable a
	case maybeMetaTable of
		Just metaTable -> do
			maybeMetaMethod <- liftPrim $ HT.lookup metaTable (LuaString opName)
			case maybeMetaMethod of
				Just _metaMethod -> fail "calling unary metamethods is not implemented yet"
				Nothing -> other
		Nothing -> other

{-# INLINABLE tryBinaryMetaMethod #-}
tryBinaryMetaMethod :: LuaMonad m => T.Text -> LuaValue m -> LuaValue m -> m (LuaValue m)
tryBinaryMetaMethod opName a b = tryBinaryMetaMethodOr opName a b $ throwLuaError $ LuaBadOperation opName

{-# INLINABLE tryBinaryMetaMethodOr #-}
tryBinaryMetaMethodOr :: LuaMonad m => T.Text -> LuaValue m -> LuaValue m -> m (LuaValue m) -> m (LuaValue m)
tryBinaryMetaMethodOr opName a b other = do
	maybeMetaTable <- do
		maybeMetaTableA <- getMetaTable a
		case maybeMetaTableA of
			Just metaTable -> return $ Just metaTable
			Nothing -> getMetaTable b
	case maybeMetaTable of
		Just metaTable -> do
			maybeMetaMethod <- liftPrim $ HT.lookup metaTable (LuaString opName)
			case maybeMetaMethod of
				Just _metaMethod -> fail "caling binary metamethods is not implemented yet"
				Nothing -> other
		Nothing -> other

{-# INLINABLE numberBinaryOp #-}
numberBinaryOp :: LuaMonad m => (Double -> Double -> Double) -> T.Text -> LuaValue m -> LuaValue m -> m (LuaValue m)
numberBinaryOp op opName a b = do
	let ma = luaCoerceToNumber a
	let mb = luaCoerceToNumber b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaReal $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINABLE integerBinaryOp #-}
integerBinaryOp :: LuaMonad m => (Int -> Int -> Int) -> T.Text -> LuaValue m -> LuaValue m -> m (LuaValue m)
integerBinaryOp op opName a b = do
	let ma = luaCoerceToInt a
	let mb = luaCoerceToInt b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaInteger $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINABLE integerOrNumberBinaryOp #-}
integerOrNumberBinaryOp :: LuaMonad m => (Int -> Int -> Int) -> (Double -> Double -> Double) -> T.Text -> LuaValue m -> LuaValue m -> m (LuaValue m)
integerOrNumberBinaryOp integerOp numberOp opName a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaInteger $ integerOp na nb
	_ -> numberBinaryOp numberOp opName a b

{-# INLINABLE luaValueAdd #-}
luaValueAdd :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueAdd = integerOrNumberBinaryOp (+) (+) "__add"

{-# INLINABLE luaValueSub #-}
luaValueSub :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueSub = integerOrNumberBinaryOp (-) (-) "__sub"

{-# INLINABLE luaValueMul #-}
luaValueMul :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueMul = integerOrNumberBinaryOp (*) (*) "__mul"

{-# INLINABLE luaValueMod #-}
luaValueMod :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueMod = integerOrNumberBinaryOp rem irem "__mod" where
	irem a b = a - fromIntegral ((truncate $ a / b) :: Int) * b

{-# INLINABLE luaValuePow #-}
luaValuePow :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValuePow = numberBinaryOp pow "__pow" where
	pow a b = exp $ log a * b

{-# INLINABLE luaValueDiv #-}
luaValueDiv :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueDiv = numberBinaryOp (/) "__div"

{-# INLINABLE luaValueIDiv #-}
luaValueIDiv :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueIDiv = integerOrNumberBinaryOp quot iquot "__idiv" where
	iquot a b = fromIntegral ((truncate $ a / b) :: Int)

{-# INLINABLE luaValueBAnd #-}
luaValueBAnd :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueBAnd = integerBinaryOp (.&.) "__band"

{-# INLINABLE luaValueBOr #-}
luaValueBOr :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueBOr = integerBinaryOp (.|.) "__bor"

{-# INLINABLE luaValueBXor #-}
luaValueBXor :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueBXor = integerBinaryOp xor "__bxor"

{-# INLINABLE luaValueShl #-}
luaValueShl :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueShl = integerBinaryOp shiftL "__shl"

{-# INLINABLE luaValueShr #-}
luaValueShr :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueShr = integerBinaryOp shiftR "__shr"

{-# INLINABLE luaValueUnm #-}
luaValueUnm :: LuaMonad m => LuaValue m -> m (LuaValue m)
luaValueUnm a = case a of
	LuaInteger n -> return $ LuaInteger $ negate n
	_ -> case luaCoerceToNumber a of
		Just n -> return $ LuaReal $ negate n
		Nothing -> tryUnaryMetaMethod "__unm" a

{-# INLINABLE luaValueBNot #-}
luaValueBNot :: LuaMonad m => LuaValue m -> m (LuaValue m)
luaValueBNot a = case luaCoerceToInt a of
	Just n -> return $ LuaInteger $ complement n
	Nothing -> tryUnaryMetaMethod "__bnot" a

{-# INLINABLE luaValueNot #-}
luaValueNot :: LuaMonad m => LuaValue m -> m (LuaValue m)
luaValueNot a = return $ LuaBoolean $ luaCoerceToBool a `xor` 1

{-# INLINABLE luaValueLen #-}
luaValueLen :: LuaMonad m => LuaValue m -> m (LuaValue m)
luaValueLen a = case a of
	LuaString s -> return $ LuaInteger $ T.length s
	_ -> tryUnaryMetaMethodOr "__len" a $ case a of
		LuaTable
			{ luaTableLength = lenVar
			} -> LuaInteger <$> readMutVar lenVar
		_ -> throwLuaError $ LuaBadOperation "__len"

{-# INLINABLE luaValueConcat #-}
luaValueConcat :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueConcat a b = case (luaCoerceToString a, luaCoerceToString b) of
	(Just sa, Just sb) -> return $ LuaString $ sa <> sb
	_ -> tryBinaryMetaMethod "__concat" a b

{-# INLINABLE luaValueEq #-}
luaValueEq :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueEq a b = if a == b then return $ LuaBoolean 1
	else fmap (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__eq" a b $ return $ LuaBoolean 0

{-# INLINABLE luaValueLt #-}
luaValueLt :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueLt a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ if na < nb then 1 else 0
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ if na < nb then 1 else 0
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ if na < nb then 1 else 0
		_ -> (LuaBoolean . luaCoerceToBool) <$> tryBinaryMetaMethod "__lt" a b

{-# INLINABLE luaValueLe #-}
luaValueLe :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueLe a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ if na <= nb then 1 else 0
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ if na <= nb then 1 else 0
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ if na <= nb then 1 else 0
		_ -> fmap (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__le" a b $
			(LuaBoolean . (`xor` 1) . luaCoerceToBool) <$> tryBinaryMetaMethod "__lt" b a

{-# INLINABLE luaValueGet #-}
luaValueGet :: LuaMonad m => LuaValue m -> LuaValue m -> m (LuaValue m)
luaValueGet t i = case t of
	LuaTable
		{ luaTable = tt
		, luaTableMetaTable = mtVar
		} -> do
		mv <- liftPrim $ HT.lookup tt i
		case mv of
			Just v -> return v
			Nothing -> do
				mt <- readMutVar mtVar
				case mt of
					LuaTable
						{ luaTable = mtt
						} -> do
						mmm <- liftPrim $ HT.lookup mtt $ LuaString "__index"
						case mmm of
							Just mm -> case mm of
								LuaClosure
									{ luaClosure = c
									} -> head <$> c [t, i]
								nt@LuaTable {} -> luaValueGet nt i
								_ -> throwLuaError $ LuaBadOperation "__index"
							Nothing -> return LuaNil
					_ -> return LuaNil
	_ -> throwLuaError $ LuaBadOperation "__index"

{-# INLINABLE luaValueSet #-}
luaValueSet :: LuaMonad m => LuaValue m -> LuaValue m -> LuaValue m -> m ()
luaValueSet t i v = case t of
	LuaTable
		{ luaTable = tt
		, luaTableLength = lenVar
		, luaTableMetaTable = mtVar
		} -> do

		let setExisting = case v of
			LuaNil -> do
				liftPrim $ HT.delete tt i
				modifyMutVar' lenVar (+ (-1))
			_ -> liftPrim $ HT.insert tt i v

		let setNew = case v of
			LuaNil -> return ()
			_ -> do
				liftPrim $ HT.insert tt i v
				modifyMutVar' lenVar (+ 1)

		mv <- liftPrim $ HT.lookup tt i
		case mv of
			Just _ -> setExisting
			Nothing -> do
				mt <- readMutVar mtVar
				case mt of
					LuaTable
						{ luaTable = mtt
						} -> do
						mmm <- liftPrim $ HT.lookup mtt $ LuaString "__newindex"
						case mmm of
							Just mm -> case mm of
								LuaClosure
									{ luaClosure = c
									} -> void $ c [t, i, v]
								nt@LuaTable {} -> luaValueSet nt i v
								_ -> throwLuaError $ LuaBadOperation "__newindex"
							Nothing -> setNew
					_ -> setNew
	_ -> throwLuaError $ LuaBadOperation "__newindex"

{-# INLINABLE luaValueCall #-}
luaValueCall :: LuaMonad m => LuaValue m -> [LuaValue m] -> m [LuaValue m]
luaValueCall func args = case func of
	LuaClosure
		{ luaClosure = f
		} -> f args
	LuaTable
		{ luaTableMetaTable = mtVar
		} -> do
		mt <- readMutVar mtVar
		case mt of
			LuaTable
				{ luaTable = mtt
				} -> do
				mmm <- liftPrim $ HT.lookup mtt $ LuaString "__call"
				case mmm of
					Just mm -> luaValueCall mm $ func : args
					Nothing -> throwLuaError $ LuaBadOperation "__call"
			_ -> throwLuaError $ LuaBadOperation "__call"
	_ -> throwLuaError $ LuaBadOperation "__call"

{-# INLINABLE luaNewTable #-}
luaNewTable :: LuaMonad m => m (LuaValue m)
luaNewTable = luaCreateTable =<< liftPrim HT.new

{-# INLINABLE luaNewTableSized #-}
luaNewTableSized :: LuaMonad m => Int -> m (LuaValue m)
luaNewTableSized size = luaCreateTable =<< liftPrim (HT.newSized size)

{-# INLINE luaCreateTable #-}
luaCreateTable :: LuaMonad m => HT.HashTable (PrimState m) (LuaValue m) (LuaValue m) -> m (LuaValue m)
luaCreateTable t = do
	u <- newLuaUnique
	lenVar <- newMutVar 0
	mtVar <- newMutVar LuaNil
	return LuaTable
		{ luaTableUnique = u
		, luaTable = t
		, luaTableLength = lenVar
		, luaTableMetaTable = mtVar
		}

{-# INLINABLE luaNewClosure #-}
luaNewClosure :: LuaMonad m => ([LuaValue m] -> m [LuaValue m]) -> m (LuaValue m)
luaNewClosure f = do
	u <- newLuaUnique
	return LuaClosure
		{ luaClosureUnique = u
		, luaClosure = f
		}
