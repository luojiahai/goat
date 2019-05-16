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


module Analyze where

import GoatAST
import SymTable


analyze :: Program -> [SymTable]
analyze (Program procs) = 
  case aProcs procs tables of
    True -> tables
    False -> error $ "InternalError"
  where tables = symTable procs

aCheckMain :: [SymTable] -> Bool
aCheckMain tables =
  case stMainSymTable tables of
    Just (SymTable header prmts hashMap) -> 
      case length prmts == 0 of
        True -> True
        False -> error $ "SemanticError: Main procedure surplus arguments"
    Nothing -> error $ "SemanticError: Main procedure not found"

aCheckDuplicateProc :: [SymTable] -> Bool
aCheckDuplicateProc tables = 
  case stDuplicate [] tables of
    False -> True
    True -> error $ "SemanticError: Duplicate procedures"

aProcs :: [Procedure] -> [SymTable] -> Bool
aProcs [] tables = True
aProcs [proc] tables = 
  (aProc proc tables) && (aCheckMain tables) && (aCheckDuplicateProc tables)
aProcs (proc:procs) tables = (aProc proc tables) && (aProcs procs tables)

aProc :: Procedure -> [SymTable] -> Bool
aProc (Procedure pos ident prmts decls stmts) tables = 
  case stLookupSymTable ident tables of
    Just table -> aStmts stmts tables table
    Nothing -> error $ "SemanticError: Undefined procedure " ++ ident

aStmts :: [Stmt] -> [SymTable] -> SymTable -> Bool
aStmts [] _ table = True
aStmts [stmt] tables table = aStmt stmt tables table
aStmts (stmt:stmts) tables table = 
  (aStmt stmt tables table) && (aStmts stmts tables table)

aStmt :: Stmt -> [SymTable] -> SymTable -> Bool
aStmt (Assign pos lvalue expr) tables table = 
  (aLvalue lvalue tables table) && (aExpr expr tables table)
aStmt (Read pos lvalue) tables table = 
  aLvalue lvalue tables table
aStmt (Write pos expr) tables table = 
  aExpr expr tables table
aStmt (ProcCall pos ident exprs) tables table = 
  (aCall ident exprs tables table) && (aExprs exprs tables table)
aStmt (If pos expr stmts) tables table = 
  (aExpr expr tables table) && (aStmts stmts tables table)
aStmt (IfElse pos expr stmts1 stmts2) tables table = 
  (aExpr expr tables table) && (aStmts stmts1 tables table) 
  && (aStmts stmts2 tables table)
aStmt (While pos expr stmts) tables table = 
  (aExpr expr tables table) && (aStmts stmts tables table)

aExprs :: [Expr] -> [SymTable] -> SymTable -> Bool
aExprs [] _ table = True
aExprs [expr] tables table = aExpr expr tables table
aExprs (expr:exprs) tables table = 
  (aExpr expr tables table) && (aExprs exprs tables table)

aExpr :: Expr -> [SymTable] -> SymTable -> Bool
aExpr (BoolCon pos const) tables table = True
aExpr (And pos expr1 expr2) tables table = True
aExpr (Or pos expr1 expr2) tables table = True
aExpr (Not pos expr) tables table = True
aExpr (Rel pos relOp expr1 expr2) tables table = True
aExpr (IntCon pos const) tables table = True
aExpr (FloatCon pos const) tables table = True
aExpr (StrCon pos const) tables table = True
aExpr (Id pos ident) tables table = True
aExpr (ArrayRef pos ident expr) tables table = True
aExpr (MatrixRef pos ident expr1 expr2) tables table = True
aExpr (BinOpExp pos binOp expr1 expr2) tables table = True
aExpr (UnaryMinus pos expr) tables table = True

aLvalue :: Lvalue -> [SymTable] -> SymTable -> Bool
aLvalue (LId pos ident) tables table = aIdBase ident tables table
aLvalue (LArrayRef pos ident expr) tables table = 
  aIdArray ident tables table
aLvalue (LMatrixRef pos ident expr1 expr2) tables table = 
  aIdMatrix ident tables table

aIdBase :: Ident -> [SymTable] -> SymTable -> Bool
aIdBase ident tables (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just value -> 
      case stAGoatType value of
        Just (AGoatType goatType) -> case goatType of
          (Base baseType) -> True
          otherwise ->
            error $ "SemanticError: Wrong GoatType for " ++ ident
              ++ ", expected: Base"
        Nothing -> error $ "InternalError: No AGoatType"
    Nothing -> error $ "SemanticError: Undefined variable " ++ ident

aIdArray :: Ident -> [SymTable] -> SymTable -> Bool
aIdArray ident tables (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just value -> 
      case stAGoatType value of
        Just (AGoatType goatType) -> case goatType of
          (Array baseType i) -> True
          otherwise ->
            error $ "SemanticError: Wrong GoatType for " ++ ident
              ++ ", expected: Array"
        Nothing -> error $ "InternalError: No AGoatType"
    Nothing -> error $ "SemanticError: Undefined variable " ++ ident

aIdMatrix :: Ident -> [SymTable] -> SymTable -> Bool
aIdMatrix ident tables (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just value -> 
      case stAGoatType value of
        Just (AGoatType goatType) -> case goatType of
          (Matrix baseType i j) -> True
          otherwise ->
            error $ "SemanticError: Wrong GoatType for " ++ ident
              ++ ", expected: Matrix"
        Nothing -> error $ "InternalError: No AGoatType"
    Nothing -> error $ "SemanticError: Undefined variable " ++ ident
  
aCall :: String -> [Expr] -> [SymTable] -> SymTable -> Bool
aCall ident exprs tables table = 
  case stLookupSymTable ident tables of
    Just (SymTable ident prmts hashMap) -> 
      if length exprs == length prmts then True
      else error $ "SemanticError: Incorrect number of arguments"
        ++ " when calling " ++ ident
        ++ ", actual " ++ show (length exprs)
        ++ ", expected " ++ show (length prmts)
    Nothing -> error $ "SemanticError: Undefined procedure " ++ ident