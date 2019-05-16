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
  ASlot Int | APos Pos | AParMode ParMode 
  | AType BaseType | AValue Expr | AGoatType GoatType
  deriving (Show, Eq)


symTable :: [Procedure] -> [SymTable]
symTable procs = stProcs procs

stDuplicate :: [SymTable] -> [SymTable] -> Bool
stDuplicate seen [] = False
stDuplicate seen (x:xs) = 
  stElem x seen || stDuplicate (x:seen) xs

stElem :: SymTable -> [SymTable] -> Bool
stElem _ [] = False
stElem (SymTable header _ _) [(SymTable header' _ _)] =
  if header == header' then True else False
stElem (SymTable header prmts hashMap) ((SymTable header' _ _):tables) =
  if header == header' then True 
  else stElem (SymTable header prmts hashMap) tables

stMainSymTable :: [SymTable] -> Maybe SymTable
stMainSymTable [] = Nothing
stMainSymTable [(SymTable header prmts hashMap)] = 
  if header == "main" then Just (SymTable header prmts hashMap)
  else Nothing
stMainSymTable ((SymTable header prmts hashMap):tables) =
  if header == "main" then Just (SymTable header prmts hashMap)
  else stMainSymTable tables

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

stBind :: String -> Symbol -> HashMap -> HashMap
stBind key value hashMap =
  case Data.Map.lookup key hashMap of 
    Just value -> error $ "SemanticError: Duplicate variable " ++ key
    Nothing -> let hashMap' = Data.Map.insert key value hashMap in hashMap'

stLookupHashMap :: String -> HashMap -> Maybe Symbol
stLookupHashMap key hashMap = Data.Map.lookup key hashMap

stLookupSymTable :: String -> [SymTable] -> Maybe SymTable
stLookupSymTable ident [] = Nothing
stLookupSymTable ident [(SymTable header prmts hashMap)] = 
  if ident == header then Just (SymTable header prmts hashMap)
  else Nothing
stLookupSymTable ident ((SymTable header prmts hashMap):tables) =
  if ident == header then Just (SymTable header prmts hashMap)
  else stLookupSymTable ident tables 

stProcs :: [Procedure] -> [SymTable]
stProcs [] = []
stProcs [proc] = [stProc proc]
stProcs (proc:procs) = stProc proc : (stProcs procs)

stProc :: Procedure -> SymTable
stProc (Procedure pos ident prmts decls stmts) = 
  let hashMap = (stPrmts prmts . stDecls decls) emptyHashMap
  in SymTable ident prmts hashMap
  where emptyHashMap = Data.Map.fromList([])

stPrmts :: [FormalArgSpec] -> HashMap -> HashMap
stPrmts [] hashMap = hashMap
stPrmts [prmt] hashMap = stPrmt prmt hashMap
stPrmts (prmt:prmts) hashMap = (stPrmt prmt . stPrmts prmts) hashMap

stPrmt :: FormalArgSpec -> HashMap -> HashMap
stPrmt (FormalArgSpec pos parMode baseType ident) hashMap = 
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [APos pos, AParMode parMode, AType baseType]

stDecls :: [Decl] -> HashMap -> HashMap
stDecls [] hashMap = hashMap
stDecls [decl] hashMap = stDecl decl hashMap
stDecls (decl:decls) hashMap = (stDecl decl . stDecls decls) hashMap

stDecl :: Decl -> HashMap -> HashMap
stDecl (Decl pos ident (Base baseType)) hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [APos pos, AType baseType, AGoatType (Base baseType)]
stDecl (Decl pos ident (Array baseType i)) hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [APos pos, AType baseType, AGoatType (Array baseType i)]
stDecl (Decl pos ident (Matrix baseType i j)) hashMap =
  let newHashMap = stBind ident symbol hashMap
  in newHashMap
  where symbol = [APos pos, AType baseType, AGoatType (Matrix baseType i j)]