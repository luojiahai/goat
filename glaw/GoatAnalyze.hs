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


module GoatAnalyze where

import GoatParser
import GoatAST
import GoatSymTable as ST

import qualified Data.Map


analyze :: GoatProgram -> [SymTable]
analyze (GoatProgram procs) = aProcs procs

aProcs :: [Procedure] -> [SymTable]
aProcs [] = []
aProcs [proc] = [aProc proc]
aProcs (proc:procs) = aProc proc : (aProcs procs)

aProc :: Procedure -> SymTable
aProc (Procedure ident prmts decls stmts) = 
  let hashMap = (aPrmts prmts . aDecls decls) emptyHashMap
  in SymTable ident hashMap
  where emptyHashMap = Data.Map.fromList([])

aPrmts :: [Prmt] -> HashMap -> HashMap
aPrmts [] hashMap = hashMap
aPrmts [prmt] hashMap = aPrmt prmt hashMap
aPrmts (prmt:prmts) hashMap = (aPrmt prmt . aPrmts prmts) hashMap

aPrmt :: Prmt -> HashMap -> HashMap
aPrmt (Prmt Val basetype name) hashMap = 
  let (_, newHashMap) = ST.bind name (Symbol name attribute) hashMap
  in newHashMap
  where attribute = [Type basetype]
aPrmt (Prmt Ref basetype name) hashMap = 
  let (_, newHashMap) = ST.bind name (Symbol name attribute) hashMap
  in newHashMap
  where attribute = [Type basetype]

aDecls :: [Decl] -> HashMap -> HashMap
aDecls [] hashMap = hashMap
aDecls [decl] hashMap = aDecl decl hashMap
aDecls (decl:decls) hashMap = (aDecl decl . aDecls decls) hashMap

aDecl :: Decl -> HashMap -> HashMap
aDecl (Decl (Ident name) basetype) hashMap =
  let (_, newHashMap) = ST.bind name (Symbol name attribute) hashMap
  in newHashMap
  where attribute = [Type basetype]
aDecl (Decl (IdentWithShape name expr) basetype) hashMap =
  let (_, newHashMap) = ST.bind name (Symbol name attribute) hashMap
  in newHashMap
  where attribute = [Type basetype]