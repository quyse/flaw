{-# LANGUAGE TemplateHaskell #-}

module Flaw.TH
	( genEnum
	, genStruct
	) where

import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

-- | Generate enum data type based on names and numbers.
genEnum :: Q Type -> String -> [(String, Int)] -> Q [Dec]
genEnum underlyingType typeName es = do
	let dataCons = [NormalC (mkName eName) [] |  (eName, _) <- es]
	let dataDec = DataD [] (mkName typeName) [] dataCons [''Show]
	p <- newName "p"
	let fromEnumDec = FunD 'fromEnum [Clause [VarP p] (NormalB $ CaseE (VarE p) [Match (ConP (mkName eName) []) (NormalB $ LitE $ IntegerL $ fromIntegral eNum) [] | (eName, eNum) <- es]) []]
	let toEnumDec = FunD 'toEnum [Clause [VarP p] (NormalB $ CaseE (VarE p) $ [Match (LitP $ IntegerL $ fromIntegral eNum) (NormalB $ ConE (mkName eName)) [] | (eName, eNum) <- es] ++ [Match WildP (NormalB $ ConE $ mkName $ fst $ head es) []]) []]
	let enumDec = InstanceD [] (AppT (ConT ''Enum) (ConT $ mkName typeName)) [toEnumDec, fromEnumDec]
	storableDecs <- [d|
		instance Storable $(conT $ mkName typeName) where
			sizeOf _ = sizeOf (undefined :: $(underlyingType))
			alignment _ = alignment (undefined :: $(underlyingType))
			peek addr = liftM (toEnum . fromIntegral) (peek (castPtr addr) :: IO $(underlyingType))
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

processField :: String -> TypeQ -> String -> ExpQ -> Q Field
processField typeName ft fn prevOffsetExp = do
	let baseName = typeName ++ "_" ++ fn
	let name = mkName $ "f_" ++ baseName
	let sizeOfName = mkName $ "field_sizeOf_" ++ baseName
	let alignmentName = mkName $ "field_alignment_" ++ baseName
	let offsetName = mkName $ "field_offset_" ++ baseName
	let addrName = mkName $ "field_addr_" ++ baseName
	let peekName = mkName $ "field_peek_" ++ baseName
	let pokeName = mkName $ "field_poke_" ++ baseName
	addrParam <- newName "addr"
	let decs =
		[ sigD sizeOfName [t| Int |]
		, valD (varP sizeOfName) (normalB [| sizeOf (undefined :: $(ft)) |]) []
		, sigD alignmentName [t| Int |]
		, valD (varP alignmentName) (normalB [| alignment (undefined :: $(ft)) |]) []
		, sigD offsetName [t| Int |]
		, valD (varP offsetName) (normalB [| (($(prevOffsetExp) + $(varE alignmentName) - 1) `div` $(varE alignmentName)) * $(varE alignmentName) |]) []
		, sigD addrName [t| Ptr $(conT $ mkName typeName) -> Ptr $(ft) |]
		, funD addrName [clause [varP addrParam] (normalB [| plusPtr (castPtr $(varE addrParam)) $(varE offsetName) |]) []]
		, sigD peekName [t| Ptr $(conT $ mkName typeName) -> IO $(ft) |]
		, funD peekName [clause [varP addrParam] (normalB [| peek ($(varE addrName) $(varE addrParam)) |]) []]
		, sigD pokeName [t| Ptr $(conT $ mkName typeName) -> $(ft) -> IO () |]
		, funD pokeName [clause [varP addrParam] (normalB [| poke ($(varE addrName) $(varE addrParam)) |]) []]
		]
	return Field
		{ fieldNameStr = fn
		, fieldName = name
		, fieldType = ft
		, fieldSizeOf = varE sizeOfName
		, fieldAlignment = varE alignmentName
		, fieldOffset = varE offsetName
		, fieldEnd = [| $(varE offsetName) + $(varE sizeOfName) |]
		, fieldPeek = varE peekName
		, fieldPoke = varE pokeName
		, fieldDecs = decs
		}

processFields :: String -> [(TypeQ, String)] -> Q [Field]
processFields typeName fs = pf [| 0 |] fs where
	pf prevOffsetExp ((ft, fn) : nfs) = do
		f <- processField typeName ft fn prevOffsetExp
		nextFields <- pf (fieldEnd f) nfs
		return $ f : nextFields
	pf _ [] = return []

-- | Generate struct data type.
genStruct :: String -> [(Q Type, String)] -> Q [Dec]
genStruct typeName fs = do
	fields <- processFields typeName fs
	let dataFieldDec field = do
		ft <- fieldType field
		return (fieldName field, NotStrict, ft)
	dataDec <- dataD (return []) (mkName typeName) [] [recC (mkName typeName) $ map dataFieldDec fields] [''Show]
	fieldsDecs <- liftM concat $ sequence $ map (sequence . fieldDecs) fields
	let sizeOfName = mkName $ "struct_sizeOf_" ++ typeName
	let alignmentName = mkName $ "struct_alignment_" ++ typeName
	structDecs <- sequence
		[ sigD sizeOfName [t| Int |]
		, valD (varP sizeOfName) (normalB $ foldl1 (\a b -> [| $(a) + $(b) |]) $ map fieldSizeOf fields) []
		, sigD alignmentName [t| Int |]
		, valD (varP alignmentName) (normalB $ foldl1 (\a b -> [| lcm $(a) $(b) |]) $ map fieldAlignment fields) []
		]
	let peekFieldDec addr field = do
		varName <- newName $ "tmp_" ++ fieldNameStr field
		return (return (fieldName field, VarE varName), bindS (varP varName) $ appE (fieldPeek field) addr)
	let peekDec addr = do
		decs <- mapM (peekFieldDec addr) fields
		doE $ (map snd decs) ++ [noBindS $ appE (varE 'return) $ recConE (mkName typeName) $ map fst decs]

	instanceDecs <- [d|
		instance Storable $(conT $ mkName typeName) where
			sizeOf _ = $(varE sizeOfName)
			alignment _ = $(varE alignmentName)
			peek addr = $(peekDec [| addr |])
			poke addr value = $(doE [noBindS $ appE (appE (fieldPoke field) [| addr |]) $ appE (varE $ fieldName field) [| value |] | field <- fields])
		|]
	return $ dataDec : fieldsDecs ++ structDecs ++ instanceDecs
