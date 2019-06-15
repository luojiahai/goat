---------------------------------------------------------------------
-- COMP90045 Programming Language Implementation                   --
-- Programming Project: Goat                                       --
--                                                                 --
-- Team: GOAT SIMULATOR                                            --
-- Members:                                                        --
--          Chenqin Zhang, Geoffrey Ka-Hoi Law, Yun Chen           --
--          733301, 759218, 760419                                 --
--          {chenqinz, glaw, yunc4}@student.unimelb.edu.au         --
---------------------------------------------------------------------


module SymTable where

import GoatAST
import qualified Data.Map


type HashMap = Data.Map.Map String Symbol

type Symbol = [Attribute]

type Header = String

data SymTable = SymTable Header [FormalArgSpec] HashMap
  deriving (Show, Eq)

data Attribute = 
  ASlot Int | APos Pos | AParMode ParMode | AArgIndex Int
  | AType BaseType | AValue Expr | AGoatType GoatType
  deriving (Show, Eq)


-- generate symbol tables given a goat program
symTable :: Program -> [SymTable]
symTable (Program procs) = stProcs procs

-- get hashmap size
stHashMapSize :: HashMap -> Int
stHashMapSize hashMap = 
  let f symbol len = len + (stSymbolSize symbol)
  in foldr f 0 hashMap

-- get variable size
stSymbolSize :: Symbol -> Int
stSymbolSize symbol = 
  case stAGoatType symbol of
    Just (AGoatType (Base baseType)) -> 1
    Just (AGoatType (Array baseType i)) -> i
    Just (AGoatType (Matrix baseType i j)) -> i * j
    Nothing -> error $ "InternalError: No AGoatType"

-- check duplication of symbol table
stDuplicate :: [SymTable] -> [SymTable] -> Bool
stDuplicate seen [] = False
stDuplicate seen (x:xs) = 
  stElem x seen || stDuplicate (x:seen) xs

-- check if one symbol table is in symbol tables
stElem :: SymTable -> [SymTable] -> Bool
stElem _ [] = False
stElem (SymTable header _ _) [(SymTable header' _ _)] =
  if header == header' then True else False
stElem (SymTable header prmts hashMap) ((SymTable header' _ _):tables) =
  if header == header' then True 
  else stElem (SymTable header prmts hashMap) tables

-- get main symbol table
stMainSymTable :: [SymTable] -> Maybe SymTable
stMainSymTable [] = Nothing
stMainSymTable [(SymTable header prmts hashMap)] = 
  if header == "main" then Just (SymTable header prmts hashMap)
  else Nothing
stMainSymTable ((SymTable header prmts hashMap):tables) =
  if header == "main" then Just (SymTable header prmts hashMap)
  else stMainSymTable tables

-- get type attribute given symbol
stAType :: Symbol -> Maybe Attribute
stAType [] = Nothing
stAType [attr] = 
  case attr of
    (AType baseType) -> Just (AType baseType)
    otherwise -> Nothing
stAType (attr:attrs) = 
  case attr of
    (AType baseType) -> Just (AType baseType)
    otherwise -> stAType attrs

-- get slot number attribute given symbol
stASlot :: Symbol -> Maybe Attribute
stASlot [] = Nothing
stASlot [attr] = 
  case attr of
    (ASlot slot) -> Just (ASlot slot)
    otherwise -> Nothing
stASlot (attr:attrs) = 
  case attr of
    (ASlot slot) -> Just (ASlot slot)
    otherwise -> stASlot attrs

-- get goat type attribute given symbol
stAGoatType :: Symbol -> Maybe Attribute
stAGoatType [] = Nothing
stAGoatType [attr] = 
  case attr of
    (AGoatType goatType) -> Just (AGoatType goatType)
    otherwise -> Nothing
stAGoatType (attr:attrs) = 
  case attr of
    (AGoatType goatType) -> Just (AGoatType goatType)
    otherwise -> stAGoatType attrs

-- get parmode attribute given symbol
stAParMode :: Symbol -> Maybe Attribute
stAParMode [] = Nothing
stAParMode [attr] = 
  case attr of
    (AParMode parMode) -> Just (AParMode parMode)
    otherwise -> Nothing
stAParMode (attr:attrs) = 
  case attr of
    (AParMode parMode) -> Just (AParMode parMode)
    otherwise -> stAParMode attrs

-- get argument index attribute given symbol
stAArgIndex :: Symbol -> Maybe Attribute
stAArgIndex [] = Nothing
stAArgIndex [attr] = 
  case attr of
    (AArgIndex argIndex) -> Just (AArgIndex argIndex)
    otherwise -> Nothing
stAArgIndex (attr:attrs) = 
  case attr of
    (AArgIndex argIndex) -> Just (AArgIndex argIndex)
    otherwise -> stAArgIndex attrs

-- get symbol given argument index
stGetArgSymbol :: Int -> SymTable -> Symbol
stGetArgSymbol i (SymTable header prmts hashMap) =
  stGetArgSymbol' i (Data.Map.toList hashMap)

stGetArgSymbol' :: Int -> [(String, Symbol)] -> Symbol
stGetArgSymbol' i [] = error $ "InternalError: Arg not found"
stGetArgSymbol' i [(k, symbol)] =
  case stAArgIndex symbol of
    Just (AArgIndex argIndex) -> 
      if argIndex == i then symbol else error $ "InternalError: Arg not found"
    Nothing -> error $ "InternalError: Arg not found"
stGetArgSymbol' i ((k, symbol):pairs) =
  case stAArgIndex symbol of
    Just (AArgIndex argIndex) -> 
      if argIndex == i then symbol else stGetArgSymbol' i pairs
    Nothing -> stGetArgSymbol' i pairs

-- bind key value pair into hashmap
stBind :: String -> Symbol -> HashMap -> HashMap
stBind key value hashMap =
  case Data.Map.lookup key hashMap of 
    Just value -> error $ "SemanticError: Duplicate variable " ++ key
    Nothing -> let hashMap' = Data.Map.insert key value hashMap in hashMap'

-- lookup hashmap
stLookupHashMap :: String -> SymTable -> Maybe Symbol
stLookupHashMap key (SymTable header prmts hashMap) = 
  Data.Map.lookup key hashMap

-- lookup symbol table
stLookupSymTable :: String -> [SymTable] -> Maybe SymTable
stLookupSymTable ident [] = Nothing
stLookupSymTable ident [(SymTable header prmts hashMap)] = 
  if ident == header then Just (SymTable header prmts hashMap)
  else Nothing
stLookupSymTable ident ((SymTable header prmts hashMap):tables) =
  if ident == header then Just (SymTable header prmts hashMap)
  else stLookupSymTable ident tables 

-- get symbol tables given a sequence of procedures
stProcs :: [Procedure] -> [SymTable]
stProcs [] = []
stProcs [proc] = [stProc proc]
stProcs (proc:procs) = stProc proc : (stProcs procs)

-- get symbol table given a procedure
stProc :: Procedure -> SymTable
stProc (Procedure pos ident prmts decls stmts) = 
  let hashMap = (stPrmts prmts 0 0 . stDecls decls (length prmts)) emptyHashMap
  in SymTable ident prmts hashMap
  where emptyHashMap = Data.Map.fromList([])

-- get hashmap given a sequence of parameters
stPrmts :: [FormalArgSpec] -> Int -> Int -> HashMap -> HashMap
stPrmts [] _ _ hashMap = hashMap
stPrmts [prmt] i slot hashMap = stPrmt prmt i slot hashMap
stPrmts (prmt:prmts) i slot hashMap = 
  (stPrmt prmt i slot . stPrmts prmts (i + 1) (slot + 1)) hashMap

-- get hashmap given a parameter while binding the parameter
stPrmt :: FormalArgSpec -> Int -> Int -> HashMap -> HashMap
stPrmt (FormalArgSpec pos parMode baseType ident) i slot hashMap = 
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [ASlot slot, APos pos, 
                  AParMode parMode, AArgIndex i,
                  AType baseType, AGoatType (Base baseType)]

-- get hashmap given a sequence of declarations
stDecls :: [Decl] -> Int -> HashMap -> HashMap
stDecls [] _ hashMap = hashMap
stDecls [decl] slot hashMap = stDecl decl slot hashMap
stDecls (decl:decls) slot hashMap = 
  (stDecl decl slot . stDecls decls (slot + offset)) hashMap
  where 
    offset = 
      case decl of
        (Decl pos ident (Base baseType)) -> 1
        (Decl pos ident (Array baseType i)) -> i
        (Decl pos ident (Matrix baseType i j)) -> (i * j)

-- get hashmap given a declaration while binding the declaration
stDecl :: Decl -> Int -> HashMap -> HashMap
stDecl (Decl pos ident (Base baseType)) slot hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [ASlot slot, APos pos, 
                  AType baseType, AGoatType (Base baseType)]
stDecl (Decl pos ident (Array baseType i)) slot hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [ASlot slot, APos pos, 
                  AType baseType, AGoatType (Array baseType i)]
stDecl (Decl pos ident (Matrix baseType i j)) slot hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [ASlot slot, APos pos, 
                  AType baseType, AGoatType (Matrix baseType i j)]