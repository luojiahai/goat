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
analyze (GoatProgram procs) = aProcs procs

aProcs :: [Procedure] -> [SymTable]
aProcs [] = []
aProcs [proc] = [aProc proc]
aProcs (proc:procs) = aProc proc : (aProcs procs)

aProc :: Procedure -> SymTable
aProc (Procedure ident prmts decls stmts) = 
  let symTable = symTableProc (Procedure ident prmts decls stmts)
  in aStmts stmts symTable

aStmts :: [Stmt] -> SymTable -> SymTable
aStmts [] symTable = symTable
aStmts [stmt] symTable = aStmt stmt symTable
aStmts (stmt:stmts) symTable = (aStmt stmt . aStmts stmts) symTable

aStmt :: Stmt -> SymTable -> SymTable
aStmt (Assign lvalue expr) symTable = 
  (aLvalue lvalue . aExpr expr) symTable
aStmt (Read lvalue) symTable = 
  aLvalue lvalue symTable
aStmt (Write expr) symTable = 
  aExpr expr symTable
aStmt (Call name exprs) symTable = 
  (aName name . aExprs exprs) symTable
aStmt (IfThen expr stmts) symTable = 
  (aExpr expr . aStmts stmts) symTable
aStmt (IfThenElse expr stmts1 stmts2) symTable = 
  (aExpr expr . aStmts stmts1 . aStmts stmts2) symTable
aStmt (While expr stmts) symTable = 
  (aExpr expr . aStmts stmts) symTable

aExprs :: [Expr] -> SymTable -> SymTable
aExprs [] symTable = symTable
aExprs [expr] symTable = aExpr expr symTable
aExprs (expr:exprs) symTable = (aExpr expr . aExprs exprs) symTable

aExpr :: Expr -> SymTable -> SymTable
aExpr (IntConst _) symTable                = symTable
aExpr (StrConst _) symTable                = symTable
aExpr (FloatConst _) symTable              = symTable
aExpr (BoolConst _) symTable               = symTable
aExpr (Id ident) symTable                  = aIdent ident symTable
aExpr (UnExpr unop expr) symTable          = aExpr expr symTable
aExpr (BinExpr binop expr1 expr2) symTable = 
  (aExpr expr1 . aExpr expr2) symTable

aLvalue :: Lvalue -> SymTable -> SymTable
aLvalue (LId ident) symTable = aIdent ident symTable

aIdent :: Ident -> SymTable -> SymTable
aIdent (Ident name) symTable = 
  aName name symTable
aIdent (IdentWithShape name exprs) symTable = 
  (aName name . aExprs exprs) symTable

aName :: String -> SymTable -> SymTable
aName name (SymTable header hashMap) = 
  let (value, newHashMap) = stLookup name hashMap
  in SymTable header newHashMap