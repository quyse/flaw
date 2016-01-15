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

luaCoerceToNumber :: LuaValue -> Maybe Double
luaCoerceToNumber v = case v of
	LuaInteger i -> Just $ fromIntegral i
	LuaReal r -> Just r
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

luaCoerceToInt :: LuaValue -> Maybe Int
luaCoerceToInt v = case v of
	LuaInteger i -> Just i
	LuaReal r -> let i = floor r in if r == fromIntegral i then Just i else Nothing
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

luaCoerceToBool :: LuaValue -> Bool
luaCoerceToBool v = case v of
	LuaNil -> False
	LuaBoolean b -> b
	_ -> True

luaCoerceToString :: LuaValue -> Maybe T.Text
luaCoerceToString v = case v of
	LuaInteger i -> Just $ T.pack $ show i
	LuaReal r -> Just $ T.pack $ show r
	LuaString s -> Just s
	_ -> Nothing

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

tryUnaryMetaMethod :: T.Text -> LuaValue -> IO LuaValue
tryUnaryMetaMethod opName a = tryUnaryMetaMethodOr opName a $ throwIO $ LuaBadOperation opName

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

tryBinaryMetaMethod :: T.Text -> LuaValue -> LuaValue -> IO LuaValue
tryBinaryMetaMethod opName a b = tryBinaryMetaMethodOr opName a b $ throwIO $ LuaBadOperation opName

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

numberBinaryOp :: (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
numberBinaryOp op opName a b = do
	let ma = luaCoerceToNumber a
	let mb = luaCoerceToNumber b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaReal $ op na nb
		_ -> tryBinaryMetaMethod opName a b

integerBinaryOp :: (Int -> Int -> Int) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerBinaryOp op opName a b = do
	let ma = luaCoerceToInt a
	let mb = luaCoerceToInt b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaInteger $ op na nb
		_ -> tryBinaryMetaMethod opName a b

integerOrNumberBinaryOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerOrNumberBinaryOp integerOp numberOp opName a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaInteger $ integerOp na nb
	_ -> numberBinaryOp numberOp opName a b

luaValueAdd :: LuaValue -> LuaValue -> IO LuaValue
luaValueAdd = integerOrNumberBinaryOp (+) (+) "__add"

luaValueSub :: LuaValue -> LuaValue -> IO LuaValue
luaValueSub = integerOrNumberBinaryOp (-) (-) "__sub"

luaValueMul :: LuaValue -> LuaValue -> IO LuaValue
luaValueMul = integerOrNumberBinaryOp (*) (*) "__mul"

luaValueMod :: LuaValue -> LuaValue -> IO LuaValue
luaValueMod = integerOrNumberBinaryOp rem irem "__mod" where
	irem a b = a - fromIntegral ((truncate $ a / b) :: Int) * b

luaValuePow :: LuaValue -> LuaValue -> IO LuaValue
luaValuePow = numberBinaryOp pow "__pow" where
	pow a b = exp $ log a * b

luaValueDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueDiv = numberBinaryOp (/) "__div"

luaValueIDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueIDiv = integerOrNumberBinaryOp quot iquot "__idiv" where
	iquot a b = fromIntegral ((truncate $ a / b) :: Int)

luaValueBAnd :: LuaValue -> LuaValue -> IO LuaValue
luaValueBAnd = integerBinaryOp (.&.) "__band"

luaValueBOr :: LuaValue -> LuaValue -> IO LuaValue
luaValueBOr = integerBinaryOp (.|.) "__bor"

luaValueBXor :: LuaValue -> LuaValue -> IO LuaValue
luaValueBXor = integerBinaryOp xor "__bxor"

luaValueShl :: LuaValue -> LuaValue -> IO LuaValue
luaValueShl = integerBinaryOp shiftL "__shl"

luaValueShr :: LuaValue -> LuaValue -> IO LuaValue
luaValueShr = integerBinaryOp shiftR "__shr"

luaValueUnm :: LuaValue -> IO LuaValue
luaValueUnm a = case a of
	LuaInteger n -> return $ LuaInteger $ negate n
	_ -> case luaCoerceToNumber a of
		Just n -> return $ LuaReal $ negate n
		Nothing -> tryUnaryMetaMethod "__unm" a

luaValueBNot :: LuaValue -> IO LuaValue
luaValueBNot a = case luaCoerceToInt a of
	Just n -> return $ LuaInteger $ complement n
	Nothing -> tryUnaryMetaMethod "__bnot" a

luaValueNot :: LuaValue -> IO LuaValue
luaValueNot a = return $ LuaBoolean $ not $ luaCoerceToBool a

luaValueLen :: LuaValue -> IO LuaValue
luaValueLen a = case a of
	LuaString s -> return $ LuaInteger $ T.length s
	_ -> tryUnaryMetaMethodOr "__len" a $ case a of
		LuaTable
			{ luaTable = table
			} -> liftM (LuaInteger . length) $ HT.toList table -- FIXME: slow
		_ -> throwIO $ LuaBadOperation "__len"

luaValueConcat :: LuaValue -> LuaValue -> IO LuaValue
luaValueConcat a b = case (luaCoerceToString a, luaCoerceToString b) of
	(Just sa, Just sb) -> return $ LuaString $ sa <> sb
	_ -> tryBinaryMetaMethod "__concat" a b

luaValueEq :: LuaValue -> LuaValue -> IO LuaValue
luaValueEq a b = if a == b then return $ LuaBoolean True
	else liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethod "__eq" a b

luaValueLt :: LuaValue -> LuaValue -> IO LuaValue
luaValueLt a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na < nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na < nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na < nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" a b

luaValueLe :: LuaValue -> LuaValue -> IO LuaValue
luaValueLe a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na <= nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na <= nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na <= nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__le" a b $
			liftM (LuaBoolean . not . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" b a

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

luaValueSet :: LuaValue -> LuaValue -> LuaValue -> IO ()
luaValueSet t i v = case t of
	LuaTable
		{ luaTable = tt
		, luaTableMetaTable = mtRef
		} -> do
		mv <- HT.lookup tt i
		case mv of
			Just _ -> HT.insert tt i v
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
							Nothing -> HT.insert tt i v
					_ -> HT.insert tt i v
	_ -> throwIO $ LuaBadOperation "__newindex"

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

luaNewTable :: IO LuaValue
luaNewTable = do
	u <- newUnique
	t <- HT.new
	mtRef <- newIORef LuaNil
	return LuaTable
		{ luaTableUnique = u
		, luaTable = t
		, luaTableMetaTable = mtRef
		}

luaNewTableSized :: Int -> IO LuaValue
luaNewTableSized size = do
	u <- newUnique
	t <- HT.newSized size
	mtRef <- newIORef LuaNil
	return LuaTable
		{ luaTableUnique = u
		, luaTable = t
		, luaTableMetaTable = mtRef
		}

luaNewClosure :: ([LuaValue] -> IO [LuaValue]) -> IO LuaValue
luaNewClosure f = do
	u <- newUnique
	return LuaClosure
		{ luaClosureUnique = u
		, luaClosure = f
		}
