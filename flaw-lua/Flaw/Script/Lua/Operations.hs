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

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.HashTable.IO as HT
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import Data.Unique

import Flaw.Script.Lua

{-# INLINABLE luaCoerceToNumber #-}
luaCoerceToNumber :: LuaValue -> Maybe Double
luaCoerceToNumber v = case v of
	LuaInteger i -> Just $ fromIntegral i
	LuaReal r -> Just r
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINABLE luaCoerceToInt #-}
luaCoerceToInt :: LuaValue -> Maybe Int
luaCoerceToInt v = case v of
	LuaInteger i -> Just i
	LuaReal r -> let i = floor r in if r == fromIntegral i then Just i else Nothing
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINABLE luaCoerceToBool #-}
luaCoerceToBool :: LuaValue -> Bool
luaCoerceToBool v = case v of
	LuaNil -> False
	LuaBoolean b -> b
	_ -> True

{-# INLINABLE luaCoerceToString #-}
luaCoerceToString :: LuaValue -> Maybe T.Text
luaCoerceToString v = case v of
	LuaInteger i -> Just $ T.pack $ show i
	LuaReal r -> Just $ T.pack $ show r
	LuaString s -> Just s
	_ -> Nothing

{-# INLINABLE luaValueShow #-}
luaValueShow :: LuaValue -> IO TL.Builder
luaValueShow a = case a of
	LuaNil -> return "nil"
	LuaBoolean b -> return $ if b then "true" else "false"
	LuaInteger i -> return $ TL.fromString $ show i
	LuaReal r -> return $ TL.fromString $ show r
	LuaString t -> return $ TL.fromString $ show $ T.unpack t
	LuaClosure
		{ luaClosureUnique = u
		} -> return $ "<<closure" <> (TL.fromString $ show $ hashUnique u) <> ">>"
	LuaUserData
		{ luaUserDataUnique = u
		} -> return $ "<<userdata" <> (TL.fromString $ show $ hashUnique u) <> ">>"
	LuaTable
		{ luaTable = t
		} -> do
		l <- HT.toList t
		s <- forM l $ \(k, v) -> do
			ks <- luaValueShow k
			vs <- luaValueShow v
			return $ "[ " <> ks <> " ] = " <> vs <> "; "
		return $ "{ " <> foldr (<>) "}" s

{-# INLINABLE getMetaTable #-}
getMetaTable :: LuaValue -> IO (Maybe (HT.CuckooHashTable LuaValue LuaValue))
getMetaTable v = case v of
	LuaTable
		{ luaTableMetaTable = metaTableRef
		} -> do
		metaTable <- readIORef metaTableRef
		return $ case metaTable of
			LuaTable
				{ luaTable = table
				} -> Just table
			_ -> Nothing
	_ -> return Nothing

{-# INLINABLE tryUnaryMetaMethod #-}
tryUnaryMetaMethod :: T.Text -> LuaValue -> IO LuaValue
tryUnaryMetaMethod opName a = tryUnaryMetaMethodOr opName a $ throwIO $ LuaBadOperation opName

{-# INLINABLE tryUnaryMetaMethodOr #-}
tryUnaryMetaMethodOr :: T.Text -> LuaValue -> IO LuaValue -> IO LuaValue
tryUnaryMetaMethodOr opName a other = do
	maybeMetaTable <- getMetaTable a
	case maybeMetaTable of
		Just metaTable -> do
			maybeMetaMethod <- HT.lookup metaTable (LuaString opName)
			case maybeMetaMethod of
				Just _metaMethod -> fail "calling unary metamethods is not implemented yet"
				Nothing -> other
		Nothing -> other

{-# INLINABLE tryBinaryMetaMethod #-}
tryBinaryMetaMethod :: T.Text -> LuaValue -> LuaValue -> IO LuaValue
tryBinaryMetaMethod opName a b = tryBinaryMetaMethodOr opName a b $ throwIO $ LuaBadOperation opName

{-# INLINABLE tryBinaryMetaMethodOr #-}
tryBinaryMetaMethodOr :: T.Text -> LuaValue -> LuaValue -> IO LuaValue -> IO LuaValue
tryBinaryMetaMethodOr opName a b other = do
	maybeMetaTable <- do
		maybeMetaTableA <- getMetaTable a
		case maybeMetaTableA of
			Just metaTable -> return $ Just metaTable
			Nothing -> getMetaTable b
	case maybeMetaTable of
		Just metaTable -> do
			maybeMetaMethod <- HT.lookup metaTable (LuaString opName)
			case maybeMetaMethod of
				Just _metaMethod -> fail "caling binary metamethods is not implemented yet"
				Nothing -> other
		Nothing -> other

{-# INLINABLE numberBinaryOp #-}
numberBinaryOp :: (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
numberBinaryOp op opName a b = do
	let ma = luaCoerceToNumber a
	let mb = luaCoerceToNumber b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaReal $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINABLE integerBinaryOp #-}
integerBinaryOp :: (Int -> Int -> Int) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerBinaryOp op opName a b = do
	let ma = luaCoerceToInt a
	let mb = luaCoerceToInt b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaInteger $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINABLE integerOrNumberBinaryOp #-}
integerOrNumberBinaryOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerOrNumberBinaryOp integerOp numberOp opName a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaInteger $ integerOp na nb
	_ -> numberBinaryOp numberOp opName a b

{-# INLINABLE luaValueAdd #-}
luaValueAdd :: LuaValue -> LuaValue -> IO LuaValue
luaValueAdd = integerOrNumberBinaryOp (+) (+) "__add"

{-# INLINABLE luaValueSub #-}
luaValueSub :: LuaValue -> LuaValue -> IO LuaValue
luaValueSub = integerOrNumberBinaryOp (-) (-) "__sub"

{-# INLINABLE luaValueMul #-}
luaValueMul :: LuaValue -> LuaValue -> IO LuaValue
luaValueMul = integerOrNumberBinaryOp (*) (*) "__mul"

{-# INLINABLE luaValueMod #-}
luaValueMod :: LuaValue -> LuaValue -> IO LuaValue
luaValueMod = integerOrNumberBinaryOp rem irem "__mod" where
	irem a b = a - fromIntegral ((truncate $ a / b) :: Int) * b

{-# INLINABLE luaValuePow #-}
luaValuePow :: LuaValue -> LuaValue -> IO LuaValue
luaValuePow = numberBinaryOp pow "__pow" where
	pow a b = exp $ log a * b

{-# INLINABLE luaValueDiv #-}
luaValueDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueDiv = numberBinaryOp (/) "__div"

{-# INLINABLE luaValueIDiv #-}
luaValueIDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueIDiv = integerOrNumberBinaryOp quot iquot "__idiv" where
	iquot a b = fromIntegral ((truncate $ a / b) :: Int)

{-# INLINABLE luaValueBAnd #-}
luaValueBAnd :: LuaValue -> LuaValue -> IO LuaValue
luaValueBAnd = integerBinaryOp (.&.) "__band"

{-# INLINABLE luaValueBOr #-}
luaValueBOr :: LuaValue -> LuaValue -> IO LuaValue
luaValueBOr = integerBinaryOp (.|.) "__bor"

{-# INLINABLE luaValueBXor #-}
luaValueBXor :: LuaValue -> LuaValue -> IO LuaValue
luaValueBXor = integerBinaryOp xor "__bxor"

{-# INLINABLE luaValueShl #-}
luaValueShl :: LuaValue -> LuaValue -> IO LuaValue
luaValueShl = integerBinaryOp shiftL "__shl"

{-# INLINABLE luaValueShr #-}
luaValueShr :: LuaValue -> LuaValue -> IO LuaValue
luaValueShr = integerBinaryOp shiftR "__shr"

{-# INLINABLE luaValueUnm #-}
luaValueUnm :: LuaValue -> IO LuaValue
luaValueUnm a = case a of
	LuaInteger n -> return $ LuaInteger $ negate n
	_ -> case luaCoerceToNumber a of
		Just n -> return $ LuaReal $ negate n
		Nothing -> tryUnaryMetaMethod "__unm" a

{-# INLINABLE luaValueBNot #-}
luaValueBNot :: LuaValue -> IO LuaValue
luaValueBNot a = case luaCoerceToInt a of
	Just n -> return $ LuaInteger $ complement n
	Nothing -> tryUnaryMetaMethod "__bnot" a

{-# INLINABLE luaValueNot #-}
luaValueNot :: LuaValue -> IO LuaValue
luaValueNot a = return $ LuaBoolean $ not $ luaCoerceToBool a

{-# INLINABLE luaValueLen #-}
luaValueLen :: LuaValue -> IO LuaValue
luaValueLen a = case a of
	LuaString s -> return $ LuaInteger $ T.length s
	_ -> tryUnaryMetaMethodOr "__len" a $ case a of
		LuaTable
			{ luaTableLength = lenRef
			} -> liftM LuaInteger $ readIORef lenRef
		_ -> throwIO $ LuaBadOperation "__len"

{-# INLINABLE luaValueConcat #-}
luaValueConcat :: LuaValue -> LuaValue -> IO LuaValue
luaValueConcat a b = case (luaCoerceToString a, luaCoerceToString b) of
	(Just sa, Just sb) -> return $ LuaString $ sa <> sb
	_ -> tryBinaryMetaMethod "__concat" a b

{-# INLINABLE luaValueEq #-}
luaValueEq :: LuaValue -> LuaValue -> IO LuaValue
luaValueEq a b = if a == b then return $ LuaBoolean True
	else liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__eq" a b $ return $ LuaBoolean False

{-# INLINABLE luaValueLt #-}
luaValueLt :: LuaValue -> LuaValue -> IO LuaValue
luaValueLt a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na < nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na < nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na < nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" a b

{-# INLINABLE luaValueLe #-}
luaValueLe :: LuaValue -> LuaValue -> IO LuaValue
luaValueLe a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na <= nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na <= nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na <= nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__le" a b $
			liftM (LuaBoolean . not . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" b a

{-# INLINABLE luaValueGet #-}
luaValueGet :: LuaValue -> LuaValue -> IO LuaValue
luaValueGet t i = case t of
	LuaTable
		{ luaTable = tt
		, luaTableMetaTable = mtRef
		} -> do
		mv <- HT.lookup tt i
		case mv of
			Just v -> return v
			Nothing -> do
				mt <- readIORef mtRef
				case mt of
					LuaTable
						{ luaTable = mtt
						} -> do
						mmm <- HT.lookup mtt $ LuaString "__index"
						case mmm of
							Just mm -> case mm of
								LuaClosure
									{ luaClosure = c
									} -> liftM head $ c [t, i]
								nt@LuaTable {} -> luaValueGet nt i
								_ -> throwIO $ LuaBadOperation "__index"
							Nothing -> return LuaNil
					_ -> return LuaNil
	_ -> throwIO $ LuaBadOperation "__index"

{-# INLINABLE luaValueSet #-}
luaValueSet :: LuaValue -> LuaValue -> LuaValue -> IO ()
luaValueSet t i v = case t of
	LuaTable
		{ luaTable = tt
		, luaTableLength = lenRef
		, luaTableMetaTable = mtRef
		} -> do

		let setExisting = case v of
			LuaNil -> do
				HT.delete tt i
				modifyIORef' lenRef (+ (-1))
			_ -> HT.insert tt i v

		let setNew = case v of
			LuaNil -> return ()
			_ -> do
				HT.insert tt i v
				modifyIORef' lenRef (+ 1)

		mv <- HT.lookup tt i
		case mv of
			Just _ -> setExisting
			Nothing -> do
				mt <- readIORef mtRef
				case mt of
					LuaTable
						{ luaTable = mtt
						} -> do
						mmm <- HT.lookup mtt $ LuaString "__newindex"
						case mmm of
							Just mm -> case mm of
								LuaClosure
									{ luaClosure = c
									} -> void $ c [t, i, v]
								nt@LuaTable {} -> luaValueSet nt i v
								_ -> throwIO $ LuaBadOperation "__newindex"
							Nothing -> setNew
					_ -> setNew
	_ -> throwIO $ LuaBadOperation "__newindex"

{-# INLINABLE luaValueCall #-}
luaValueCall :: LuaValue -> [LuaValue] -> IO [LuaValue]
luaValueCall func args = case func of
	LuaClosure
		{ luaClosure = f
		} -> f args
	LuaTable
		{ luaTableMetaTable = mtRef
		} -> do
		mt <- readIORef mtRef
		case mt of
			LuaTable
				{ luaTable = mtt
				} -> do
				mmm <- HT.lookup mtt $ LuaString "__call"
				case mmm of
					Just mm -> luaValueCall mm $ func : args
					Nothing -> throwIO $ LuaBadOperation "__call"
			_ -> throwIO $ LuaBadOperation "__call"
	_ -> throwIO $ LuaBadOperation "__call"

{-# INLINABLE luaNewTable #-}
luaNewTable :: IO LuaValue
{-# INLINABLE luaNewTableSized #-}
luaNewTableSized :: Int -> IO LuaValue
(luaNewTable, luaNewTableSized) = let
	create t = do
		u <- newUnique
		lenRef <- newIORef 0
		mtRef <- newIORef LuaNil
		return LuaTable
			{ luaTableUnique = u
			, luaTable = t
			, luaTableLength = lenRef
			, luaTableMetaTable = mtRef
			}
	in (create =<< HT.new, \size -> create =<< HT.newSized size)

{-# INLINABLE luaNewClosure #-}
luaNewClosure :: ([LuaValue] -> IO [LuaValue]) -> IO LuaValue
luaNewClosure f = do
	u <- newUnique
	return LuaClosure
		{ luaClosureUnique = u
		, luaClosure = f
		}
