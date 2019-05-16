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
  case (aCheckMain tables) && (aCheckDuplicateProc tables) of
    True -> tables
    False -> error $ "InternalError"
  where tables = aProcs procs (symTable procs)

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

aProcs :: [Procedure] -> [SymTable] -> [SymTable]
aProcs [] _ = []
aProcs [proc] tables = [aProc proc tables]
aProcs (proc:procs) tables = aProc proc tables : (aProcs procs tables)

aProc :: Procedure -> [SymTable] -> SymTable
aProc (Procedure pos ident prmts decls stmts) tables = 
  case stLookupSymTable ident tables of
    Just table -> aStmts stmts tables table
    Nothing -> error $ "SemanticError: Undefined procedure " ++ ident

aStmts :: [Stmt] -> [SymTable] -> SymTable -> SymTable
aStmts [] _ table = table
aStmts [stmt] tables table = aStmt stmt tables table
aStmts (stmt:stmts) tables table = 
  (aStmt stmt tables . aStmts stmts tables) table

aStmt :: Stmt -> [SymTable] -> SymTable -> SymTable
aStmt (Assign pos lvalue expr) tables table = 
  (aLvalue lvalue tables . aExpr expr tables) table
aStmt (Read pos lvalue) tables table = 
  aLvalue lvalue tables table
aStmt (Write pos expr) tables table = 
  aExpr expr tables table
aStmt (ProcCall pos name exprs) tables table = 
  (aCall name exprs tables . aExprs exprs tables) table
aStmt (If pos expr stmts) tables table = 
  (aExpr expr tables . aStmts stmts tables) table
aStmt (IfElse pos expr stmts1 stmts2) tables table = 
  (aExpr expr tables . aStmts stmts1 tables . aStmts stmts2 tables) table
aStmt (While pos expr stmts) tables table = 
  (aExpr expr tables . aStmts stmts tables) table

aExprs :: [Expr] -> [SymTable] -> SymTable -> SymTable
aExprs [] _ table = table
aExprs [expr] tables table = aExpr expr tables table
aExprs (expr:exprs) tables table = 
  (aExpr expr tables . aExprs exprs tables) table

aExpr :: Expr -> [SymTable] -> SymTable -> SymTable
aExpr (BoolCon pos const) tables table = table
aExpr (And pos expr1 expr2) tables table = table
aExpr (Or pos expr1 expr2) tables table = table
aExpr (Not pos expr) tables table = table
aExpr (Rel pos relOp expr1 expr2) tables table = table
aExpr (IntCon pos const) tables table = table
aExpr (FloatCon pos const) tables table = table
aExpr (StrCon pos const) tables table = table
aExpr (Id pos ident) tables table = table
aExpr (ArrayRef pos ident expr) tables table = table
aExpr (MatrixRef pos ident expr1 expr2) tables table = table
aExpr (BinOpExp pos binOp expr1 expr2) tables table = table
aExpr (UnaryMinus pos expr) tables table = table

aLvalue :: Lvalue -> [SymTable] -> SymTable -> SymTable
aLvalue (LId pos ident) tables table = table
aLvalue (LArrayRef pos ident expr) tables table = table
aLvalue (LMatrixRef pos ident expr1 expr2) tables table = table

-- aIdentName :: String -> [SymTable] -> SymTable -> SymTable
-- aIdentName name tables (SymTable header prmts hashMap) = 
--   case stLookupHashMap name hashMap of
--     Just value -> 
--       case stAttrId value of
--         Just (AId ident) -> case ident of
--           (IdentWithShape name' exprs') ->
--             error $ "SemanticError: " ++ name 
--               ++ ", actual no shape"
--               ++ ", expected " ++ show (length exprs')
--           otherwise -> SymTable header prmts hashMap
--         Nothing -> error $ "InternalError: No AId"
--     Nothing -> error $ "SemanticError: Undefined variable " ++ name

-- aIdentNameWithShape :: String -> [Expr] -> [SymTable] -> SymTable -> SymTable
-- aIdentNameWithShape name exprs tables (SymTable header prmts hashMap) = 
--   case stLookupHashMap name hashMap of
--     Just value -> 
--       case stAttrId value of
--         Just (AId ident) -> case ident of
--           (IdentWithShape name' exprs') -> 
--             if length exprs == length exprs'
--             then SymTable header prmts hashMap 
--             else error $ "SemanticError: " ++ name 
--               ++ ", actual " ++ show (length exprs)
--               ++ ", expected " ++ show (length exprs')
--           otherwise -> error $ "SemanticError: " ++ name 
--             ++ ", actual " ++ show (length exprs)
--             ++ ", expected no shape"
--         Nothing -> error $ "InternalError: No AId"
--     Nothing -> error $ "SemanticError: Undefined variable " ++ name
  
aCall :: String -> [Expr] -> [SymTable] -> SymTable -> SymTable
aCall name exprs tables table = 
  case stLookupSymTable name tables of
    Just (SymTable ident prmts hashMap) -> 
      if length exprs == length prmts then table
      else error $ "SemanticError: Incorrect number of arguments"
        ++ " when calling " ++ ident
        ++ ", actual " ++ show (length exprs)
        ++ ", expected " ++ show (length prmts)
    Nothing -> error $ "SemanticError: Undefined procedure " ++ name