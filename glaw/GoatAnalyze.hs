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
  let table = symTable (Procedure ident prmts decls stmts)
  in aStmts stmts table

aStmts :: [Stmt] -> SymTable -> SymTable
aStmts [] table = table
aStmts [stmt] table = aStmt stmt table
aStmts (stmt:stmts) table = (aStmt stmt . aStmts stmts) table

aStmt :: Stmt -> SymTable -> SymTable
aStmt (Assign lvalue expr) table = 
  (aLvalue lvalue . aExpr expr) table
aStmt (Read lvalue) table = 
  aLvalue lvalue table
aStmt (Write expr) table = 
  aExpr expr table
aStmt (Call name exprs) table = 
  (aName name . aExprs exprs) table
aStmt (IfThen expr stmts) table = 
  (aExpr expr . aStmts stmts) table
aStmt (IfThenElse expr stmts1 stmts2) table = 
  (aExpr expr . aStmts stmts1 . aStmts stmts2) table
aStmt (While expr stmts) table = 
  (aExpr expr . aStmts stmts) table

aExprs :: [Expr] -> SymTable -> SymTable
aExprs [] table = table
aExprs [expr] table = aExpr expr table
aExprs (expr:exprs) table = (aExpr expr . aExprs exprs) table

aExpr :: Expr -> SymTable -> SymTable
aExpr (IntConst _) table                = table
aExpr (StrConst _) table                = table
aExpr (FloatConst _) table              = table
aExpr (BoolConst _) table               = table
aExpr (Id ident) table                  = aIdent ident table
aExpr (UnExpr unop expr) table          = aExpr expr table
aExpr (BinExpr binop expr1 expr2) table = 
  (aExpr expr1 . aExpr expr2) table

aLvalue :: Lvalue -> SymTable -> SymTable
aLvalue (LId ident) table = aIdent ident table

aIdent :: Ident -> SymTable -> SymTable
aIdent (Ident name) table = 
  aName name table
aIdent (IdentWithShape name exprs) table = 
  (aName name . aExprs exprs) table

aName :: String -> SymTable -> SymTable
aName name (SymTable header hashMap) = 
  let (value, newHashMap) = stLookup name hashMap
  in SymTable header newHashMap