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
  dataDec <- dataD (sequence []) (mkName typeName) [] Nothing [normalC (mkName eName) [] |  (eName, _) <- es] [derivClause Nothing [ [t| Show |], [t| Eq |], [t| Ord |] ] ]
  p <- newName "p"
  enumDec <- instanceD (sequence []) [t| Enum $(conT $ mkName typeName) |]
    [ funD 'toEnum [clause [varP p] (normalB $ caseE (varE p) $ [match (litP $ integerL $ fromIntegral eNum) (normalB $ conE (mkName eName)) [] | (eName, eNum) <- es] ++ [match wildP (normalB $ conE $ mkName $ fst $ head es) []]) []]
    , funD 'fromEnum [clause [varP p] (normalB $ caseE (varE p) [match (conP (mkName eName) []) (normalB $ litE $ integerL $ fromIntegral eNum) [] | (eName, eNum) <- es]) []]
    ]
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
pokeArrayWithLength len ptr lst =
  if listNoLongerThan lst len then
    pokeArray ptr lst
  else
    fail "list too big to poke into array"

processField :: String -> TypeQ -> String -> Int -> ExpQ -> Q Field
processField typeName ft fn fc prevEndExp = do
  addrParam <- newName "addr"
  let
    baseName = typeName ++ "_" ++ fn
    name = mkName $ "f_" ++ baseName
    sizeOfName = mkName $ "field_sizeOf_" ++ baseName
    alignmentName = mkName $ "field_alignment_" ++ baseName
    offsetName = mkName $ "field_offset_" ++ baseName
    addrName = mkName $ "field_addr_" ++ baseName
    peekName = mkName $ "field_peek_" ++ baseName
    pokeName = mkName $ "field_poke_" ++ baseName
    rft = if fc == 0 then ft else [t| [ $ft ] |]
    decs =
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
      , funD addrName [clause [varP addrParam] (normalB [| plusPtr $(varE addrParam) $(varE offsetName) |]) []]
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
processFields typeName = pf [| 0 |] where
  pf prevEndExp ((ft, fn, fc) : nfs) = do
    f <- processField typeName ft fn fc prevEndExp
    (nextFields, nextEndExp) <- pf (fieldEnd f) nfs
    return (f : nextFields, nextEndExp)
  pf prevEndExp [] = return ([], prevEndExp)

dataFieldDec :: Field -> Q (Name, Bang, Type)
dataFieldDec field = do
  ft <- fieldType field
  return (fieldName field, Bang NoSourceUnpackedness NoSourceStrictness, ft)

-- | Generate struct data type.
genStruct :: String -> [(TypeQ, String)] -> Q [Dec]
genStruct typeName fs = genStructWithArrays typeName [(t, n, 0) | (t, n) <- fs]

-- | Generate struct data type with support of array fields.
genStructWithArrays :: String -> [(TypeQ, String, Int)] -> Q [Dec]
genStructWithArrays typeName fs = do
  (fields, endExp) <- processFields typeName fs
  dataDec <- dataD (return []) (mkName typeName) [] Nothing [recC (mkName typeName) $ map dataFieldDec fields] [derivClause Nothing [ [t| Show |] ] ]
  fieldsDecs <- concat <$> mapM (sequence . fieldDecs) fields
  let
    sizeOfName = mkName $ "struct_sizeOf_" ++ typeName
    alignmentName = mkName $ "struct_alignment_" ++ typeName
  structDecs <- sequence
    [ sigD sizeOfName [t| Int |]
    , valD (varP sizeOfName) (normalB endExp) []
    , sigD alignmentName [t| Int |]
    , valD (varP alignmentName) (normalB $ foldl1 (\a b -> [| lcm $(a) $(b) |]) $ map fieldAlignment fields) []
    ]
  let
    peekFieldDec addr field = do
      varName <- newName $ "tmp_" ++ fieldNameStr field
      return (return (fieldName field, VarE varName), bindS (varP varName) $ appE (fieldPeek field) addr)
    peekDec addr = do
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
  dataDec <- dataD (return []) (mkName typeName) [] Nothing [recC (mkName $ typeName ++ "_" ++ fieldNameStr unionField) $ map dataFieldDec $ headerFields ++ [unionField] | unionField <- unionFields] [derivClause Nothing [ [t| Show |] ] ]
  headerFieldsDecs <- concat <$> mapM (sequence . fieldDecs) headerFields
  unionFieldsDecs <- concat <$> mapM (sequence . fieldDecs) unionFields
  let
    sizeOfName = mkName $ "struct_sizeOf_" ++ typeName
    alignmentName = mkName $ "struct_alignment_" ++ typeName
  structDecs <- sequence
    [ sigD sizeOfName [t| Int |]
    , valD (varP sizeOfName) (normalB $ [| maximum $(listE $ map fieldEnd unionFields) |]) []
    , sigD alignmentName [t| Int |]
    , valD (varP alignmentName) (normalB $ foldl1 (\a b -> [| lcm $(a) $(b) |]) $ map fieldAlignment (headerFields ++ unionFields)) []
    ]
  let
    peekFieldDec addr field = do
      varName <- newName $ "tmp_" ++ fieldNameStr field
      return (varName, return (fieldName field, VarE varName), bindS (varP varName) $ appE (fieldPeek field) addr)
    peekDec addr = do
      headerDecs <- mapM (peekFieldDec addr) headerFields
      let
        makeMatch unionField (matchPatStr, _, matchName) = do
          (_un, uc, ud) <- peekFieldDec addr unionField
          match (conP (mkName matchPatStr) []) (normalB $ doE [ud, noBindS $ appE (varE 'return) $ recConE (mkName $ typeName ++ "_" ++ matchName) $ [c | (_n, c, _d) <- headerDecs] ++ [uc]]) []
        (sn, _sc, _sd) = headerDecs !! selectorIndex
        caseExp = caseE (varE sn) $ zipWith makeMatch unionFields ufs
      doE $ [d | (_n, _c, d) <- headerDecs] ++ [noBindS caseExp]

    pokeFieldDec addr field = do
      varName <- newName $ "tmp_" ++ fieldNameStr field
      return (return (fieldName field, VarP varName), noBindS $ appE (appE (fieldPoke field) addr) $ varE varName)
    pokeDec addr value = let
      makeMatch unionField = do
        decs <- mapM (pokeFieldDec addr) $ headerFields ++ [unionField]
        match (recP (mkName $ typeName ++ "_" ++ fieldNameStr unionField) $ map fst decs) (normalB $ doE $ map snd decs) []
      in caseE value $ map makeMatch unionFields

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
