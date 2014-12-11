{-|
Module: Flaw.FFI.COM
Description: Generating declarations for Windows COM interfaces.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.FFI.COM
	( COMInterface(getIID)
	, getCOMObject
	, HRESULT
	, hresultSucceeded
	, hresultFailed
	, genCOMInterface
	) where

import Data.Int
import Data.Maybe
import qualified Data.UUID as UUID
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

type HRESULT = Int32

-- | Size of pointer on target architecture.
ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

class COMInterface i where
	-- | Get IID of COM interface. Argument is not used.
	getIID :: i -> UUID.UUID
	-- | Get size of virtual table. Argument is not used.
	sizeOfCOMVirtualTable :: i -> Int
	-- | Internal method to parse table of virtual methods.
	peekCOMVirtualTable
		:: Ptr () -- ^ 'this' pointer
		-> Ptr () -- ^ pointer to table of virtual methods
		-> IO i

-- | Public interface for getting COM object from any pointer.
getCOMObject :: COMInterface i => Ptr a -> IO i
getCOMObject this = peek ((castPtr this) :: Ptr (Ptr ())) >>= peekCOMVirtualTable (castPtr this)

-- | If HRESULT value represents success.
hresultSucceeded :: HRESULT -> Bool
hresultSucceeded hr = hr >= 0

-- | If HRESULT value represents failure.
hresultFailed :: HRESULT -> Bool
hresultFailed hr = hr < 0

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
		, tySynD foreignFunctionTypeName [] [t| Ptr () -> $(conT functionTypeName) |]
		, forImpD stdCall safe "dynamic" makeName [t| FunPtr $(conT foreignFunctionTypeName) -> $(conT foreignFunctionTypeName) |]
		]
	let classDecs = \paramName comGetExp ->
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

-- | Generate necessary things for COM interface.
-- In order to appropriately export interface, let say, (genCOMInterface "IMyInterface" ...),
-- you need to export: MyModule (IMyInterface(..), IMyInterface_Classes(..)).
genCOMInterface
	:: String -- ^ Name of the interface.
	-> String -- ^ String representation of IID (interface GUID).
	-> Maybe String -- ^ Optional name of parent interface.
	-> [(TypeQ, String)] -- ^ List of methods. Type of method should be without 'this' argument.
	-> Q [Dec]
genCOMInterface interfaceNameStr iid maybeParentInterfaceName ms = do
	let interfaceName = mkName interfaceNameStr
	let iidName = mkName $ "iid_" ++ interfaceNameStr
	let endName = mkName $ "ie_" ++ interfaceNameStr
	let parentFieldName = mkName $ "pd_" ++ interfaceNameStr
	thisName <- newName "this"
	vtName <- newName "vt"
	(beginExp, parentFields, peekParentBinds, parentFieldsConstr, parentInstanceDecs) <- case maybeParentInterfaceName of
		Just parentInterfaceNameStr -> do
			let parentInterfaceName = mkName parentInterfaceNameStr
			let parentInterfaceClassName = mkName $ parentInterfaceNameStr ++ "_Class"
			let comGetParent = mkName $ "com_get_" ++ parentInterfaceNameStr
			parentParamName <- newName "parent"
			parentInstanceDec <- instanceD (return []) [t| $(conT parentInterfaceClassName) $(conT interfaceName) |]
				[ funD comGetParent [clause [recP interfaceName [return (parentFieldName, VarP parentParamName)]] (normalB [| $(varE comGetParent) $(varE parentParamName) |]) []]
				]
			return
				( [| sizeOfCOMVirtualTable (undefined :: $(conT $ mkName parentInterfaceNameStr)) |]
				, [return (parentFieldName, NotStrict, ConT parentInterfaceName)]
				, [ bindS (varP parentParamName) [| peekCOMVirtualTable $(varE thisName) $(varE vtName) |]
					]
				, [return (parentFieldName, VarE parentParamName)]
				, [parentInstanceDec]
				)
		Nothing -> return ([| 0 |], [], [], [], [])
	methods <- processMethods interfaceNameStr beginExp ms
	dataDec <- dataD (return []) interfaceName [] [recC interfaceName $ parentFields ++ map methodField methods] []
	-- instance COMInterface IInterface
	let mp1 method = do
		p <- newName $ "p_" ++ methodNameStr method
		let binding = bindS (varP p) [| peek (plusPtr $(varE vtName) $(methodOffset method)) |]
		makeExp <- [| $(methodMake method) (castPtrToFunPtr $(varE p)) $(varE thisName) |]
		let field = (methodFieldName method, makeExp)
		return (binding, field)
	mp1s <- mapM mp1 methods
	let peekCOMVirtualTableDec = funD 'peekCOMVirtualTable [clause [varP thisName, varP vtName] body []] where
		body = normalB $ doE $ peekParentBinds ++ (map fst mp1s) ++ [noBindS $ appE (varE 'return) $ recConE interfaceName $ parentFieldsConstr ++ map (return . snd) mp1s]
	comInterfaceInstanceDec <- instanceD (return []) [t| COMInterface $(conT interfaceName) |]
		[ funD 'getIID [clause [wildP] (normalB $ varE iidName) []]
		, funD 'sizeOfCOMVirtualTable [clause [wildP] (normalB $ varE endName) []]
		, peekCOMVirtualTableDec
		]
	let className = mkName $ interfaceNameStr ++ "_Class"
	let comGetName = mkName $ "com_get_" ++ interfaceNameStr
	paramName <- newName "a"
	-- class IInterface_Class a
	classDec <- classD (return []) className [PlainTV paramName] []
		((sigD comGetName [t| $(varT paramName) -> $(conT interfaceName) |]) : (concat $ map (\method -> methodClassDecs method paramName $ varE comGetName) methods))
	-- instance IInterface_Class IInterface
	instanceDec <- instanceD (return []) [t| $(conT className) $(conT interfaceName) |]
		[ valD (varP comGetName) (normalB [| id |]) []
		]
	endSigDec <- sigD endName [t| Int |]
	endValDec <- valD (varP endName) (normalB [| $beginExp + $(litE $ integerL $ fromIntegral $ length methods) * ptrSize |]) []
	iidSigDec <- sigD iidName [t| UUID.UUID |]
	iidValDec <- valD (varP iidName) (normalB [| fromJust (UUID.fromString $(litE $ stringL iid)) |]) []
	methodsTopDecs <- mapM methodTopDecs methods
	return $ dataDec : comInterfaceInstanceDec : classDec : instanceDec : endSigDec : endValDec : iidSigDec : iidValDec : (concat methodsTopDecs) ++ parentInstanceDecs
