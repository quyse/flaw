{-|
Module: Flaw.FFI
Description: Generating declarations for C++ types.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.FFI
	( EnumWrapper(..)
	, wrapEnum
	, unwrapEnum
	, genEnum
	, genStruct
	, genStructWithArrays
	, genStructWithEndUnion
	, forwardRef
	) where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

-- | Foreign-marshallable wrapper for enum types.
newtype EnumWrapper enumType = EnumWrapper Int

-- | Wrap enum value into wrapper.
wrapEnum :: Enum a => a -> EnumWrapper a
wrapEnum a = EnumWrapper $ fromEnum a
-- | Unwrap enum value from wrapper.
unwrapEnum :: Enum a => EnumWrapper a -> a
unwrapEnum (EnumWrapper a) = toEnum a

-- | Generate enum data type based on names and numbers.
genEnum :: TypeQ -> String -> [(String, Int)] -> Q [Dec]
genEnum underlyingType typeName es = do
	let dataCons = [NormalC (mkName eName) [] |  (eName, _) <- es]
	let dataDec = DataD [] (mkName typeName) [] dataCons [''Show, ''Eq, ''Ord]
	p <- newName "p"
	let fromEnumDec = FunD 'fromEnum [Clause [VarP p] (NormalB $ CaseE (VarE p) [Match (ConP (mkName eName) []) (NormalB $ LitE $ IntegerL $ fromIntegral eNum) [] | (eName, eNum) <- es]) []]
	let toEnumDec = FunD 'toEnum [Clause [VarP p] (NormalB $ CaseE (VarE p) $ [Match (LitP $ IntegerL $ fromIntegral eNum) (NormalB $ ConE (mkName eName)) [] | (eName, eNum) <- es] ++ [Match WildP (NormalB $ ConE $ mkName $ fst $ head es) []]) []]
	let enumDec = InstanceD [] (AppT (ConT ''Enum) (ConT $ mkName typeName)) [toEnumDec, fromEnumDec]
	storableDecs <- [d|
		instance Storable $(conT $ mkName typeName) where
			sizeOf _ = sizeOf (undefined :: $(underlyingType))
			alignment _ = alignment (undefined :: $(underlyingType))
			peek addr = fmap (toEnum . fromIntegral) (peek (castPtr addr) :: IO $(underlyingType))
			poke addr val = poke (castPtr addr) ((fromIntegral $ fromEnum val) :: $(underlyingType))
		|]
	return $ dataDec : enumDec : storableDecs

data Field = Field
	{ fieldNameStr :: String
	, fieldName :: Name
	, fieldType :: TypeQ
	, fieldSizeOf :: ExpQ
	, fieldAlignment :: ExpQ
	, fieldOffset :: ExpQ
	, fieldEnd :: ExpQ
	, fieldPeek :: ExpQ
	, fieldPoke :: ExpQ
	, fieldDecs :: [DecQ]
	}

-- | Check if list is no more that specified limit.
listNoLongerThan :: [a] -> Int -> Bool
listNoLongerThan (_:xs) len = len > 0 && listNoLongerThan xs (len - 1)
listNoLongerThan [] _ = True

-- | Poke array with length limit.
pokeArrayWithLength :: Storable a => Int -> Ptr a -> [a] -> IO ()
pokeArrayWithLength len ptr lst = do
	if listNoLongerThan lst len then
		pokeArray ptr lst
	else
		fail "list too big to poke into array"

processField :: String -> TypeQ -> String -> Int -> ExpQ -> Q Field
processField typeName ft fn fc prevEndExp = do
	let baseName = typeName ++ "_" ++ fn
	let name = mkName $ "f_" ++ baseName
	let sizeOfName = mkName $ "field_sizeOf_" ++ baseName
	let alignmentName = mkName $ "field_alignment_" ++ baseName
	let offsetName = mkName $ "field_offset_" ++ baseName
	let addrName = mkName $ "field_addr_" ++ baseName
	let peekName = mkName $ "field_peek_" ++ baseName
	let pokeName = mkName $ "field_poke_" ++ baseName
	addrParam <- newName "addr"
	let rft = if fc == 0 then ft else [t| [ $ft ] |]
	let decs =
		[ sigD sizeOfName [t| Int |]
		, valD (varP sizeOfName)
			( normalB $
				if fc == 0 then
					[| sizeOf (undefined :: $(ft)) |]
				else
					[| $(litE $ integerL $ fromIntegral fc) * sizeOf (undefined :: $(ft)) |]
			) []
		, sigD alignmentName [t| Int |]
		, valD (varP alignmentName) (normalB [| alignment (undefined :: $(ft)) |]) []
		, sigD offsetName [t| Int |]
		, valD (varP offsetName) (normalB [| (($(prevEndExp) + $(varE alignmentName) - 1) `quot` $(varE alignmentName)) * $(varE alignmentName) |]) []
		, sigD addrName [t| Ptr $(conT $ mkName typeName) -> Ptr $(ft) |]
		, funD addrName [clause [varP addrParam] (normalB [| plusPtr (castPtr $(varE addrParam)) $(varE offsetName) |]) []]
		, sigD peekName [t| Ptr $(conT $ mkName typeName) -> IO $(rft) |]
		, funD peekName [clause [varP addrParam]
			( normalB $
				if fc == 0 then
					[| peek ($(varE addrName) $(varE addrParam)) |]
				else
					[| peekArray $(litE $ integerL $ fromIntegral fc) ($(varE addrName) $(varE addrParam)) |]
			) []]
		, sigD pokeName [t| Ptr $(conT $ mkName typeName) -> $(rft) -> IO () |]
		, funD pokeName [clause [varP addrParam]
			( normalB $
				if fc == 0 then
					[| poke ($(varE addrName) $(varE addrParam)) |]
				else
					[| pokeArrayWithLength $(litE $ integerL $ fromIntegral fc) ($(varE addrName) $(varE addrParam)) |]
			) []]
		]
	return Field
		{ fieldNameStr = fn
		, fieldName = name
		, fieldType = rft
		, fieldSizeOf = varE sizeOfName
		, fieldAlignment = varE alignmentName
		, fieldOffset = varE offsetName
		, fieldEnd = [| $(varE offsetName) + $(varE sizeOfName) |]
		, fieldPeek = varE peekName
		, fieldPoke = varE pokeName
		, fieldDecs = decs
		}

-- | Create ''Field objects for fields.
-- Calculates offsets and other stuff, starting from zero offset, and chaining end offsets.
processFields :: String -> [(TypeQ, String, Int)] -> Q ([Field], ExpQ)
processFields typeName fs = pf [| 0 |] fs where
	pf prevEndExp ((ft, fn, fc) : nfs) = do
		f <- processField typeName ft fn fc prevEndExp
		(nextFields, nextEndExp) <- pf (fieldEnd f) nfs
		return (f : nextFields, nextEndExp)
	pf prevEndExp [] = return ([], prevEndExp)

dataFieldDec :: Field -> Q (Name, Strict, Type)
dataFieldDec field = do
	ft <- fieldType field
	return (fieldName field, NotStrict, ft)

-- | Generate struct data type.
genStruct :: String -> [(TypeQ, String)] -> Q [Dec]
genStruct typeName fs = genStructWithArrays typeName [(t, n, 0) | (t, n) <- fs]

-- | Generate struct data type with support of array fields.
genStructWithArrays :: String -> [(TypeQ, String, Int)] -> Q [Dec]
genStructWithArrays typeName fs = do
	(fields, endExp) <- processFields typeName fs
	dataDec <- dataD (return []) (mkName typeName) [] [recC (mkName typeName) $ map dataFieldDec fields] [''Show]
	fieldsDecs <- fmap concat $ mapM (sequence . fieldDecs) fields
	let sizeOfName = mkName $ "struct_sizeOf_" ++ typeName
	let alignmentName = mkName $ "struct_alignment_" ++ typeName
	structDecs <- sequence
		[ sigD sizeOfName [t| Int |]
		, valD (varP sizeOfName) (normalB endExp) []
		, sigD alignmentName [t| Int |]
		, valD (varP alignmentName) (normalB $ foldl1 (\a b -> [| lcm $(a) $(b) |]) $ map fieldAlignment fields) []
		]
	let peekFieldDec addr field = do
		varName <- newName $ "tmp_" ++ fieldNameStr field
		return (return (fieldName field, VarE varName), bindS (varP varName) $ appE (fieldPeek field) addr)
	let peekDec addr = do
		decs <- mapM (peekFieldDec addr) fields
		doE $ map snd decs ++ [noBindS $ appE (varE 'return) $ recConE (mkName typeName) $ map fst decs]

	instanceDecs <- [d|
		instance Storable $(conT $ mkName typeName) where
			sizeOf _ = $(varE sizeOfName)
			alignment _ = $(varE alignmentName)
			peek addr = $(peekDec [| addr |])
			poke addr value = $(doE [noBindS $ appE (appE (fieldPoke field) [| addr |]) $ appE (varE $ fieldName field) [| value |] | field <- fields])
		|]
	return $ dataDec : fieldsDecs ++ structDecs ++ instanceDecs

-- | Generate D3D11-style struct with union of types in the end, selected by one of the fields.
-- Generates multiple constructors selected by selector.
genStructWithEndUnion :: String -> [(TypeQ, String, Int)] -> Int -> [(String, TypeQ, String)] -> Q [Dec]
genStructWithEndUnion typeName hfs selectorIndex ufs = do
	(headerFields, headerEndExp) <- processFields typeName hfs
	unionFields <- mapM (\(_ufe, uft, ufn) -> processField typeName uft ufn 0 headerEndExp) ufs
	dataDec <- dataD (return []) (mkName typeName) [] [recC (mkName $ typeName ++ "_" ++ fieldNameStr unionField) $ map dataFieldDec $ headerFields ++ [unionField] | unionField <- unionFields] [''Show]
	headerFieldsDecs <- fmap concat $ mapM (sequence . fieldDecs) headerFields
	unionFieldsDecs <- fmap concat $ mapM (sequence . fieldDecs) unionFields
	let sizeOfName = mkName $ "struct_sizeOf_" ++ typeName
	let alignmentName = mkName $ "struct_alignment_" ++ typeName
	structDecs <- sequence
		[ sigD sizeOfName [t| Int |]
		, valD (varP sizeOfName) (normalB $ [| maximum $(listE $ map fieldEnd unionFields) |]) []
		, sigD alignmentName [t| Int |]
		, valD (varP alignmentName) (normalB $ foldl1 (\a b -> [| lcm $(a) $(b) |]) $ map fieldAlignment (headerFields ++ unionFields)) []
		]
	let peekFieldDec addr field = do
		varName <- newName $ "tmp_" ++ fieldNameStr field
		return (varName, return (fieldName field, VarE varName), bindS (varP varName) $ appE (fieldPeek field) addr)
	let peekDec addr = do
		headerDecs <- mapM (peekFieldDec addr) headerFields
		let makeMatch unionField (matchPatStr, _, matchName) = do
			(_un, uc, ud) <- peekFieldDec addr unionField
			match (conP (mkName matchPatStr) []) (normalB $ doE [ud, noBindS $ appE (varE 'return) $ recConE (mkName $ typeName ++ "_" ++ matchName) $ [c | (_n, c, _d) <- headerDecs] ++ [uc]]) []
		let (sn, _sc, _sd) = headerDecs !! selectorIndex
		let caseExp = caseE (varE sn) $ zipWith makeMatch unionFields ufs
		doE $ [d | (_n, _c, d) <- headerDecs] ++ [noBindS caseExp]

	let pokeFieldDec addr field = do
		varName <- newName $ "tmp_" ++ fieldNameStr field
		return (return (fieldName field, VarP varName), noBindS $ appE (appE (fieldPoke field) addr) $ varE varName)
	let pokeDec addr value = do
		let makeMatch unionField = do
			decs <- mapM (pokeFieldDec addr) $ headerFields ++ [unionField]
			match (recP (mkName $ typeName ++ "_" ++ fieldNameStr unionField) $ map fst decs) (normalB $ doE $ map snd decs) []
		caseE value $ map makeMatch unionFields

	instanceDecs <- [d|
		instance Storable $(conT $ mkName typeName) where
			sizeOf _ = $(varE sizeOfName)
			alignment _ = $(varE alignmentName)
			peek addr = $(peekDec [| addr |])
			poke addr value = $(pokeDec [| addr |] [| value |])
		|]

	return $ dataDec : headerFieldsDecs ++ unionFieldsDecs ++ structDecs ++ instanceDecs

-- | Make a reference to forward declaration for a type.
forwardRef :: String -> TypeQ
forwardRef name = conT $ mkName name
