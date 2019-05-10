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
import GoatSymTable


analyze :: GoatProgram -> [SymTable]
analyze (GoatProgram procs) = 
  aProcs procs tables
  where tables = symTable procs

aProcs :: [Procedure] -> [SymTable] -> [SymTable]
aProcs [] _ = []
aProcs [proc] tables = [aProc proc tables]
aProcs (proc:procs) tables = aProc proc tables : (aProcs procs tables)

aProc :: Procedure -> [SymTable] -> SymTable
aProc (Procedure ident prmts decls stmts) tables = 
  aStmts stmts tables table
  where (_, table) = stLookupSymTable ident tables table

aStmts :: [Stmt] -> [SymTable] -> SymTable -> SymTable
aStmts [] _ table = table
aStmts [stmt] tables table = aStmt stmt tables table
aStmts (stmt:stmts) tables table = 
  (aStmt stmt tables . aStmts stmts tables) table

aStmt :: Stmt -> [SymTable] -> SymTable -> SymTable
aStmt (Assign lvalue expr) tables table = 
  (aLvalue lvalue tables . aExpr expr tables) table
aStmt (Read lvalue) tables table = 
  aLvalue lvalue tables table
aStmt (Write expr) tables table = 
  aExpr expr tables table
aStmt (Call name exprs) tables table = 
  (aProcName name tables . aExprs exprs tables) table
aStmt (IfThen expr stmts) tables table = 
  (aExpr expr tables . aStmts stmts tables) table
aStmt (IfThenElse expr stmts1 stmts2) tables table = 
  (aExpr expr tables . aStmts stmts1 tables . aStmts stmts2 tables) table
aStmt (While expr stmts) tables table = 
  (aExpr expr tables . aStmts stmts tables) table

aExprs :: [Expr] -> [SymTable] -> SymTable -> SymTable
aExprs [] _ table = table
aExprs [expr] tables table = aExpr expr tables table
aExprs (expr:exprs) tables table = 
  (aExpr expr tables . aExprs exprs tables) table

aExpr :: Expr -> [SymTable] -> SymTable -> SymTable
aExpr (IntConst _) tables table = table
aExpr (StrConst _) tables table = table
aExpr (FloatConst _) tables table = table
aExpr (BoolConst _) tables table = table
aExpr (Id ident) tables table = 
  aIdent ident tables table
aExpr (UnExpr unop expr) tables table = 
  aExpr expr tables table
aExpr (BinExpr binop expr1 expr2) tables table = 
  (aExpr expr1 tables . aExpr expr2 tables) table

aLvalue :: Lvalue -> [SymTable] -> SymTable -> SymTable
aLvalue (LId ident) tables table = aIdent ident tables table

aIdent :: Ident -> [SymTable] -> SymTable -> SymTable
aIdent (Ident name) tables table = aVarName name tables table
aIdent (IdentWithShape name exprs) tables table = 
  (aVarName name tables . aExprs exprs tables) table

aVarName :: String -> [SymTable] -> SymTable -> SymTable
aVarName name tables (SymTable header prmts hashMap) = 
  let (value, newHashMap) = stLookupHashMap name hashMap
  in SymTable header prmts newHashMap

aProcName :: String -> [SymTable] -> SymTable -> SymTable
aProcName name tables table = 
  let (oldTable, newTable) = stLookupSymTable name tables table
  in oldTable