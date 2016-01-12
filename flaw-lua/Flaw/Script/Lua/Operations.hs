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
	, luaValueCall
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.HashTable.IO as HT
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

import Flaw.Script.Lua

{-# INLINEABLE luaCoerceToNumber #-}
luaCoerceToNumber :: LuaValue -> Maybe Double
luaCoerceToNumber v = case v of
	LuaInteger i -> Just $ fromIntegral i
	LuaReal r -> Just r
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINEABLE luaCoerceToInt #-}
luaCoerceToInt :: LuaValue -> Maybe Int
luaCoerceToInt v = case v of
	LuaInteger i -> Just i
	LuaReal r -> let i = floor r in if r == fromIntegral i then Just i else Nothing
	LuaString s -> case reads $ T.unpack $ T.strip s of
		[(n, "")] -> Just n
		_ -> Nothing
	_ -> Nothing

{-# INLINEABLE luaCoerceToBool #-}
luaCoerceToBool :: LuaValue -> Bool
luaCoerceToBool v = case v of
	LuaNil -> False
	LuaBoolean b -> b
	_ -> True

{-# INLINEABLE luaCoerceToString #-}
luaCoerceToString :: LuaValue -> Maybe T.Text
luaCoerceToString v = case v of
	LuaInteger i -> Just $ T.pack $ show i
	LuaReal r -> Just $ T.pack $ show r
	LuaString s -> Just s
	_ -> Nothing

{-# INLINEABLE getMetaTable #-}
getMetaTable :: LuaValue -> IO (Maybe (HT.CuckooHashTable LuaValue LuaValue))
getMetaTable v = do
	let maybeMetaTableRef = case v of
		LuaUserData
			{ luaUserDataMetaTable = metaTableRef
			} -> Just metaTableRef
		LuaTable
			{ luaTableMetaTable = metaTableRef
			} -> Just metaTableRef
		_ -> Nothing
	maybeMetaTable <- case maybeMetaTableRef of
		Just metaTableRef -> liftM Just $ readIORef metaTableRef
		Nothing -> return Nothing
	return $ case maybeMetaTable of
		Just LuaTable
			{ luaTable = table
			} -> Just table
		_ -> Nothing

{-# INLINEABLE tryUnaryMetaMethod #-}
tryUnaryMetaMethod :: T.Text -> LuaValue -> IO LuaValue
tryUnaryMetaMethod opName a = tryUnaryMetaMethodOr opName a $ throwIO LuaBadOperation

{-# INLINEABLE tryUnaryMetaMethodOr #-}
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

{-# INLINEABLE tryBinaryMetaMethod #-}
tryBinaryMetaMethod :: T.Text -> LuaValue -> LuaValue -> IO LuaValue
tryBinaryMetaMethod opName a b = tryBinaryMetaMethodOr opName a b $ throwIO LuaBadOperation

{-# INLINEABLE tryBinaryMetaMethodOr #-}
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

{-# INLINEABLE numberBinaryOp #-}
numberBinaryOp :: (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
numberBinaryOp op opName a b = do
	let ma = luaCoerceToNumber a
	let mb = luaCoerceToNumber b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaReal $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINEABLE integerBinaryOp #-}
integerBinaryOp :: (Int -> Int -> Int) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerBinaryOp op opName a b = do
	let ma = luaCoerceToInt a
	let mb = luaCoerceToInt b
	case (ma, mb) of
		(Just na, Just nb) -> return $ LuaInteger $ op na nb
		_ -> tryBinaryMetaMethod opName a b

{-# INLINEABLE integerOrNumberBinaryOp #-}
integerOrNumberBinaryOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> T.Text -> LuaValue -> LuaValue -> IO LuaValue
integerOrNumberBinaryOp integerOp numberOp opName a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaInteger $ integerOp na nb
	_ -> numberBinaryOp numberOp opName a b

{-# INLINEABLE luaValueAdd #-}
luaValueAdd :: LuaValue -> LuaValue -> IO LuaValue
luaValueAdd = integerOrNumberBinaryOp (+) (+) "__add"

{-# INLINEABLE luaValueSub #-}
luaValueSub :: LuaValue -> LuaValue -> IO LuaValue
luaValueSub = integerOrNumberBinaryOp (-) (-) "__sub"

{-# INLINEABLE luaValueMul #-}
luaValueMul :: LuaValue -> LuaValue -> IO LuaValue
luaValueMul = integerOrNumberBinaryOp (*) (*) "__mul"

{-# INLINEABLE luaValueMod #-}
luaValueMod :: LuaValue -> LuaValue -> IO LuaValue
luaValueMod = integerOrNumberBinaryOp mod imod "__mod" where
	imod a b = a - fromIntegral ((floor $ a / b) :: Int) * b

{-# INLINEABLE luaValuePow #-}
luaValuePow :: LuaValue -> LuaValue -> IO LuaValue
luaValuePow = numberBinaryOp pow "__pow" where
	pow a b = exp $ log a * b

{-# INLINEABLE luaValueDiv #-}
luaValueDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueDiv = numberBinaryOp (/) "__div"

{-# INLINEABLE luaValueIDiv #-}
luaValueIDiv :: LuaValue -> LuaValue -> IO LuaValue
luaValueIDiv = integerOrNumberBinaryOp div idiv "__idiv" where
	idiv a b = fromIntegral ((floor $ a / b) :: Int)

{-# INLINEABLE luaValueBAnd #-}
luaValueBAnd :: LuaValue -> LuaValue -> IO LuaValue
luaValueBAnd = integerBinaryOp (.&.) "__band"

{-# INLINEABLE luaValueBOr #-}
luaValueBOr :: LuaValue -> LuaValue -> IO LuaValue
luaValueBOr = integerBinaryOp (.|.) "__bor"

{-# INLINEABLE luaValueBXor #-}
luaValueBXor :: LuaValue -> LuaValue -> IO LuaValue
luaValueBXor = integerBinaryOp xor "__bxor"

{-# INLINEABLE luaValueShl #-}
luaValueShl :: LuaValue -> LuaValue -> IO LuaValue
luaValueShl = integerBinaryOp shiftL "__shl"

{-# INLINEABLE luaValueShr #-}
luaValueShr :: LuaValue -> LuaValue -> IO LuaValue
luaValueShr = integerBinaryOp shiftR "__shr"

{-# INLINEABLE luaValueUnm #-}
luaValueUnm :: LuaValue -> IO LuaValue
luaValueUnm a = case a of
	LuaInteger n -> return $ LuaInteger $ negate n
	_ -> case luaCoerceToNumber a of
		Just n -> return $ LuaReal $ negate n
		Nothing -> tryUnaryMetaMethod "__unm" a

{-# INLINEABLE luaValueBNot #-}
luaValueBNot :: LuaValue -> IO LuaValue
luaValueBNot a = case luaCoerceToInt a of
	Just n -> return $ LuaInteger $ complement n
	Nothing -> tryUnaryMetaMethod "__bnot" a

{-# INLINEABLE luaValueNot #-}
luaValueNot :: LuaValue -> IO LuaValue
luaValueNot a = return $ LuaBoolean $ not $ luaCoerceToBool a

{-# INLINEABLE luaValueLen #-}
luaValueLen :: LuaValue -> IO LuaValue
luaValueLen a = case a of
	LuaString s -> return $ LuaInteger $ T.length s
	_ -> tryUnaryMetaMethodOr "__len" a $ case a of
		LuaTable
			{ luaTable = table
			} -> liftM (LuaInteger . length) $ HT.toList table -- FIXME: slow
		_ -> throwIO LuaBadOperation

{-# INLINEABLE luaValueConcat #-}
luaValueConcat :: LuaValue -> LuaValue -> IO LuaValue
luaValueConcat a b = case (luaCoerceToString a, luaCoerceToString b) of
	(Just sa, Just sb) -> return $ LuaString $ sa <> sb
	_ -> tryBinaryMetaMethod "__concat" a b

{-# INLINEABLE luaValueEq #-}
luaValueEq :: LuaValue -> LuaValue -> IO LuaValue
luaValueEq a b = if a == b then return $ LuaBoolean True
	else liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethod "__eq" a b

{-# INLINEABLE luaValueLt #-}
luaValueLt :: LuaValue -> LuaValue -> IO LuaValue
luaValueLt a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na < nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na < nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na < nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" a b

{-# INLINEABLE luaValueLe #-}
luaValueLe :: LuaValue -> LuaValue -> IO LuaValue
luaValueLe a b = case (a, b) of
	(LuaInteger na, LuaInteger nb) -> return $ LuaBoolean $ na <= nb
	(LuaString na, LuaString nb) -> return $ LuaBoolean $ na <= nb
	_ -> case (luaCoerceToNumber a, luaCoerceToNumber b) of
		(Just na, Just nb) -> return $ LuaBoolean $ na <= nb
		_ -> liftM (LuaBoolean . luaCoerceToBool) $ tryBinaryMetaMethodOr "__le" a b $
			liftM (LuaBoolean . not . luaCoerceToBool) $ tryBinaryMetaMethod "__lt" b a

{-# INLINEABLE luaValueCall #-}
luaValueCall :: LuaValue -> LuaState -> [LuaValue] -> IO [LuaValue]
luaValueCall func state args = case func of
	LuaClosure
		{ luaClosure = f
		} -> f state args
	_ -> fail "call via metatable is not implemented yet"
