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


module GoatSymTable where

import GoatParser
import GoatAST
import qualified Data.Map


type HashMap = Data.Map.Map String Symbol

type Symbol = [Attribute]

type Header = String

data SymTable = SymTable Header [Prmt] HashMap
  deriving (Show, Eq)

data Attribute = ASlot Int | AId Ident | AType BaseType | AValue Expr 
  deriving (Show, Eq)


symTable :: [Procedure] -> [SymTable]
symTable procs = stProcs procs

stAttrId :: Symbol -> Attribute
stAttrId [] = error $ "InternalError: No AId"
stAttrId [attr] = 
  case attr of
    (AId ident) -> AId ident
    otherwise -> error $ "InternalError: No AId"
stAttrId (attr:attrs) = 
  case attr of
    (AId ident) -> AId ident
    otherwise -> stAttrId attrs

stBind :: String -> Symbol -> HashMap -> ((), HashMap)
stBind key value hashMap =
  let hashMap' = Data.Map.insert key value hashMap
  in ((), hashMap')

stLookupHashMap :: String -> HashMap -> (Symbol, HashMap)
stLookupHashMap key hashMap = 
  case Data.Map.lookup key hashMap of
    Just value -> (value, hashMap)
    Nothing -> error $ "SemanticError: Undefined variable " ++ key

stLookupSymTable :: String -> [SymTable] -> SymTable -> (SymTable, SymTable)
stLookupSymTable ident [] _ = 
  error $ "SemanticError: Undefined procedure " ++ ident
stLookupSymTable ident [(SymTable header prmts hashMap)] table = 
  if ident == header then (table, (SymTable header prmts hashMap))
  else error $ "SemanticError: Undefined procedure " ++ ident
stLookupSymTable ident ((SymTable header prmts hashMap):tables) table =
  if ident == header then (table, (SymTable header prmts hashMap))
  else stLookupSymTable ident tables table

stProcs :: [Procedure] -> [SymTable]
stProcs [] = []
stProcs [proc] = [stProc proc]
stProcs (proc:procs) = stProc proc : (stProcs procs)

stProc :: Procedure -> SymTable
stProc (Procedure ident prmts decls stmts) = 
  let hashMap = (stPrmts prmts . stDecls decls) emptyHashMap
  in SymTable ident prmts hashMap
  where emptyHashMap = Data.Map.fromList([])

stPrmts :: [Prmt] -> HashMap -> HashMap
stPrmts [] hashMap = hashMap
stPrmts [prmt] hashMap = stPrmt prmt hashMap
stPrmts (prmt:prmts) hashMap = (stPrmt prmt . stPrmts prmts) hashMap

stPrmt :: Prmt -> HashMap -> HashMap
stPrmt (Prmt Val basetype name) hashMap = 
  let (_, newHashMap) = stBind name symbol hashMap
  in newHashMap
  where symbol = [AId (Ident name), AType basetype]
stPrmt (Prmt Ref basetype name) hashMap = 
  let (_, newHashMap) = stBind name symbol hashMap
  in newHashMap
  where symbol = [AId (Ident name), AType basetype]

stDecls :: [Decl] -> HashMap -> HashMap
stDecls [] hashMap = hashMap
stDecls [decl] hashMap = stDecl decl hashMap
stDecls (decl:decls) hashMap = (stDecl decl . stDecls decls) hashMap

stDecl :: Decl -> HashMap -> HashMap
stDecl (Decl (Ident name) basetype) hashMap =
  let (_, newHashMap) = stBind name symbol hashMap
  in newHashMap
  where symbol = [AId (Ident name), AType basetype]
stDecl (Decl (IdentWithShape name exprs) basetype) hashMap =
  let (_, newHashMap) = stBind name symbol hashMap
  in newHashMap
  where symbol = [AId (IdentWithShape name exprs), AType basetype]