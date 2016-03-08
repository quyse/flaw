{-|
Module: Flaw.FFI.COM.TH
Description: Generating declarations for Windows COM interfaces.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.FFI.COM.TH
	( genCOMInterface
	) where

import Data.Maybe
import qualified Data.UUID as UUID
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

import Flaw.FFI.COM.Internal

-- | Size of pointer on target architecture.
ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

-- | Internal info about method.
data Method = Method
	{ methodNameStr :: String
	, methodName :: Name
	, methodType :: TypeQ
	, methodFieldName :: Name
	, methodOffset :: ExpQ
	, methodEnd :: ExpQ
	, methodMake :: ExpQ
	, methodTopDecs :: DecsQ
	, methodClassDecs :: Name -> ExpQ -> [DecQ]
	, methodField :: VarStrictTypeQ
	}

processMethod :: String -> TypeQ -> String -> ExpQ -> Q Method
processMethod interfaceName mt mn prevEndExp = do
	let baseName = interfaceName ++ "_" ++ mn
	let name = mkName $ "m_" ++ baseName
	let functionTypeName = mkName $ "Mft_" ++ baseName
	let foreignFunctionTypeName = mkName $ "Mfft_" ++ baseName
	let fieldName = mkName $ "mp_" ++ baseName
	let offsetName = mkName $ "method_offset_" ++ baseName
	let makeName = mkName $ "mk_" ++ baseName
	let topDecs = sequence
		[ sigD offsetName [t| Int |]
		, valD (varP offsetName) (normalB prevEndExp) []
		, tySynD functionTypeName [] mt
		, tySynD foreignFunctionTypeName [] [t| Ptr $(conT $ mkName interfaceName) -> $(conT functionTypeName) |]
		, forImpD stdCall safe "dynamic" makeName [t| FunPtr $(conT foreignFunctionTypeName) -> $(conT foreignFunctionTypeName) |]
		]
	let classDecs paramName comGetExp =
		[ sigD name [t| $(varT paramName) -> $(conT functionTypeName) |]
		, valD (varP name) (normalB [| $(varE fieldName) . $comGetExp |]) []
		]
	return Method
		{ methodNameStr = mn
		, methodName = name
		, methodType = mt
		, methodFieldName = fieldName
		, methodOffset = varE offsetName
		, methodEnd = [| $(varE offsetName) + ptrSize |]
		, methodMake = varE makeName
		, methodTopDecs = topDecs
		, methodClassDecs = classDecs
		, methodField = return (fieldName, NotStrict, ConT functionTypeName)
		}

processMethods :: String -> ExpQ -> [(TypeQ, String)] -> Q [Method]
processMethods interfaceName firstOffsetExp ms = pm firstOffsetExp ms where
	pm prevEndExp ((mt, mn) : nms) = do
		m <- processMethod interfaceName mt mn prevEndExp
		nextMethods <- pm (methodEnd m) nms
		return $ m : nextMethods
	pm _ [] = return []

-- | Create list of interface names with all parents.
getInterfaceChain :: [String] -> Q [String]
getInterfaceChain [] = return []
getInterfaceChain [nameStr] = do
	maybeParentFieldName <- lookupValueName $ "pd_" ++ nameStr
	case maybeParentFieldName of
		Just parentFieldName -> do
			VarI _ (AppT (AppT ArrowT _) (ConT parentName)) _ _ <- reify parentFieldName
			parents <- getInterfaceChain [nameBase parentName]
			return $ nameStr : parents
		Nothing -> return [nameStr]
getInterfaceChain (n:ns) = do
	rns <- getInterfaceChain ns
	return $ n:rns

-- | Generate necessary things for COM interface.
-- In order to appropriately export interface, let say, (genCOMInterface "IMyInterface" ...),
-- you need to export: MyModule (IMyInterface(..), IMyInterface_Classes(..)).
-- List of parent interfaces may be not full (like contain only immediate parent),
-- the method will try to find all parents by reification. The possibility to
-- manually specify all interfaces is useful when stage restriction is in place
-- (i.e. reify doesn't work).
genCOMInterface
	:: String -- ^ Name of the interface.
	-> String -- ^ String representation of IID (interface GUID).
	-> [String] -- ^ Names of parent interfaces, from immediate to root.
	-> [(TypeQ, String)] -- ^ List of methods. Type of method should be without 'this' argument.
	-> Q [Dec]
genCOMInterface interfaceNameStr iid parentInterfaceNames ms = do
	let interfaceName = mkName interfaceNameStr
	let iidName = mkName $ "iid_" ++ interfaceNameStr
	let endName = mkName $ "ie_" ++ interfaceNameStr
	let parentFieldName = mkName $ "pd_" ++ interfaceNameStr
	let thisName = mkName $ "it_" ++ interfaceNameStr
	thisParamName <- newName "this"
	vtParamName <- newName "vt"
	(beginExp, parentFields, peekParentBinds, parentFieldsConstr, parentInstanceDecs) <- case parentInterfaceNames of
		(firstParentInterfaceNameStr:_) -> do
			parentParamName <- newName "parent"
			let parentDec parentInterfaceNameStr = do
				let parentInterfaceClassName = mkName $ parentInterfaceNameStr ++ "_Class"
				let comGetParent = mkName $ "com_get_" ++ parentInterfaceNameStr
				instanceD (return []) [t| $(conT parentInterfaceClassName) $(conT interfaceName) |]
					[funD comGetParent [clause [recP interfaceName [return (parentFieldName, VarP parentParamName)]] (normalB [| $(varE comGetParent) $(varE parentParamName) |]) []]]
			parentDecs <- mapM parentDec =<< getInterfaceChain parentInterfaceNames
			return
				( [| sizeOfCOMVirtualTable (undefined :: $(conT $ mkName firstParentInterfaceNameStr)) |]
				, [return (parentFieldName, NotStrict, ConT $ mkName firstParentInterfaceNameStr)]
				, [bindS (varP parentParamName) [| peekCOMVirtualTable (castPtr $(varE thisParamName)) $(varE vtParamName) |]]
				, [return (parentFieldName, VarE parentParamName)]
				, parentDecs
				)
		[] -> return ([| 0 |], [], [], [], [])
	methods <- processMethods interfaceNameStr beginExp ms
	dataDec <- dataD (return []) interfaceName [] [recC interfaceName $ return (thisName, NotStrict, AppT (ConT ''Ptr) $ ConT interfaceName) : parentFields ++ map methodField methods] []

	-- instance COMInterface IInterface
	let mp1 method = do
		p <- newName $ "p_" ++ methodNameStr method
		let binding = bindS (varP p) [| peek (plusPtr $(varE vtParamName) $(methodOffset method)) |]
		makeExp <- [| $(methodMake method) (castPtrToFunPtr $(varE p)) $(varE thisParamName) |]
		let field = (methodFieldName method, makeExp)
		return (binding, field)
	mp1s <- mapM mp1 methods
	let peekCOMVirtualTableDec = funD 'peekCOMVirtualTable [clause [varP thisParamName, varP vtParamName] body []] where
		body = normalB $ doE $ peekParentBinds ++ (map fst mp1s) ++ [noBindS $ appE (varE 'return) $ recConE interfaceName $ return (thisName, VarE thisParamName) : parentFieldsConstr ++ map (return . snd) mp1s]
	comInterfaceInstanceDec <- instanceD (return []) [t| COMInterface $(conT interfaceName) |]
		[ funD 'getIID [clause [wildP] (normalB $ varE iidName) []]
		, funD 'getCOMInterfaceName [clause [wildP] (normalB $ litE $ StringL interfaceNameStr) []]
		, funD 'sizeOfCOMVirtualTable [clause [wildP] (normalB $ varE endName) []]
		, funD 'pokeCOMObject [clause [recP interfaceName [return (thisName, VarP thisParamName)]] (normalB $ varE thisParamName) []]
		, peekCOMVirtualTableDec
		]

	aParam <- newName "a"
	bParam <- newName "b"

	-- instance Eq IInterface
	eqInterfaceInstanceDec <- instanceD (return []) [t| Eq $(conT interfaceName) |]
		[ funD '(==) [clause [recP interfaceName [return (thisName, VarP aParam)], recP interfaceName [return (thisName, VarP bParam)]] (normalB [| $(varE aParam) == $(varE bParam) |]) []]
		]

	-- instance Ord IInterface
	ordInterfaceInstanceDec <- instanceD (return []) [t| Ord $(conT interfaceName) |]
		[ funD 'compare [clause [recP interfaceName [return (thisName, VarP aParam)], recP interfaceName [return (thisName, VarP bParam)]] (normalB [| compare $(varE aParam) $(varE bParam) |]) []]
		, funD '(<=) [clause [recP interfaceName [return (thisName, VarP aParam)], recP interfaceName [return (thisName, VarP bParam)]] (normalB [| $(varE aParam) <= $(varE bParam) |]) []]
		]

	let className = mkName $ interfaceNameStr ++ "_Class"
	let comGetName = mkName $ "com_get_" ++ interfaceNameStr
	paramName <- newName "a"
	-- class IInterface_Class a
	classDec <- classD (sequence [ [t| COMInterface $(varT paramName) |] ]) className [PlainTV paramName] []
		((sigD comGetName [t| $(varT paramName) -> $(conT interfaceName) |]) : (concatMap (\method -> methodClassDecs method paramName $ varE comGetName) methods))
	-- instance IInterface_Class IInterface
	instanceDec <- instanceD (return []) [t| $(conT className) $(conT interfaceName) |]
		[ valD (varP comGetName) (normalB [| id |]) []
		]
	endSigDec <- sigD endName [t| Int |]
	endValDec <- valD (varP endName) (normalB [| $beginExp + $(litE $ integerL $ fromIntegral $ length methods) * ptrSize |]) []
	iidSigDec <- sigD iidName [t| IID |]
	iidValDec <- valD (varP iidName) (normalB [| fromJust (UUID.fromString $(litE $ stringL iid)) |]) []
	methodsTopDecs <- mapM methodTopDecs methods
	return $ dataDec : comInterfaceInstanceDec : eqInterfaceInstanceDec : ordInterfaceInstanceDec : classDec : instanceDec : endSigDec : endValDec : iidSigDec : iidValDec : (concat methodsTopDecs) ++ parentInstanceDecs
