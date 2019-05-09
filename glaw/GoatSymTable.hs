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

type Header = String

data SymTable = SymTable Header HashMap
    deriving (Show, Eq)

data Attribute = Slot Int | Type BaseType
    deriving (Show, Eq)

data Symbol = Symbol String [Attribute]
    deriving (Show, Eq)

symTable :: GoatProgram -> [SymTable]
symTable (GoatProgram procs) = stProcs procs

stBind :: String -> Symbol -> HashMap -> ((), HashMap)
stBind key value hashMap =
    let hashMap' = Data.Map.insert key value hashMap
    in ((), hashMap')

stLookup :: String -> HashMap -> (Symbol, HashMap)
stLookup key hashMap = 
    case Data.Map.lookup key hashMap of
      Just value -> (value, hashMap)
      Nothing -> error $ "Undefined variable " ++ key

stProcs :: [Procedure] -> [SymTable]
stProcs [] = []
stProcs [proc] = [stProc proc]
stProcs (proc:procs) = stProc proc : (stProcs procs)

stProc :: Procedure -> SymTable
stProc (Procedure ident prmts decls stmts) = 
  let hashMap = (stPrmts prmts . stDecls decls) emptyHashMap
  in SymTable ident hashMap
  where emptyHashMap = Data.Map.fromList([])

stPrmts :: [Prmt] -> HashMap -> HashMap
stPrmts [] hashMap = hashMap
stPrmts [prmt] hashMap = stPrmt prmt hashMap
stPrmts (prmt:prmts) hashMap = (stPrmt prmt . stPrmts prmts) hashMap

stPrmt :: Prmt -> HashMap -> HashMap
stPrmt (Prmt Val basetype name) hashMap = 
  let (_, newHashMap) = stBind name (Symbol name attributes) hashMap
  in newHashMap
  where attributes = [Type basetype]
stPrmt (Prmt Ref basetype name) hashMap = 
  let (_, newHashMap) = stBind name (Symbol name attributes) hashMap
  in newHashMap
  where attributes = [Type basetype]

stDecls :: [Decl] -> HashMap -> HashMap
stDecls [] hashMap = hashMap
stDecls [decl] hashMap = stDecl decl hashMap
stDecls (decl:decls) hashMap = (stDecl decl . stDecls decls) hashMap

stDecl :: Decl -> HashMap -> HashMap
stDecl (Decl (Ident name) basetype) hashMap =
  let (_, newHashMap) = stBind name (Symbol name attributes) hashMap
  in newHashMap
  where attributes = [Type basetype]
stDecl (Decl (IdentWithShape name expr) basetype) hashMap =
  let (_, newHashMap) = stBind name (Symbol name attributes) hashMap
  in newHashMap
  where attributes = [Type basetype]