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
  case (aCheckMain tables) && (aCheckDuplicate tables) of
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

aCheckDuplicate :: [SymTable] -> Bool
aCheckDuplicate tables = 
  case stDuplicate [] tables of
    False -> True
    True -> error $ "SemanticError: Duplicate procedures"

aProcs :: [Procedure] -> [SymTable] -> [SymTable]
aProcs [] _ = []
aProcs [proc] tables = [aProc proc tables]
aProcs (proc:procs) tables = aProc proc tables : (aProcs procs tables)

aProc :: Procedure -> [SymTable] -> SymTable
aProc (Procedure ident prmts decls stmts) tables = 
  case stLookupSymTable ident tables of
    Just table -> aStmts stmts tables table
    Nothing -> error $ "SemanticError: Undefined procedure " ++ ident

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
  (aCall name exprs tables . aExprs exprs tables) table
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
aExpr (Id ident) tables table = aIdent ident tables table
aExpr (UnExpr unop expr) tables table = aExpr expr tables table
aExpr (BinExpr binop expr1 expr2) tables table = 
  (aExpr expr1 tables . aExpr expr2 tables) table

aLvalue :: Lvalue -> [SymTable] -> SymTable -> SymTable
aLvalue (LId ident) tables table = aIdent ident tables table

aIdent :: Ident -> [SymTable] -> SymTable -> SymTable
aIdent (Ident name) tables table = aIdentName name tables table
aIdent (IdentWithShape name exprs) tables table = 
  (aIdentNameWithShape name exprs tables . aExprs exprs tables) table

aIdentName :: String -> [SymTable] -> SymTable -> SymTable
aIdentName name tables (SymTable header prmts hashMap) = 
  case stLookupHashMap name hashMap of
    Just value -> 
      case stAttrId value of
        Just (AId ident) -> case ident of
          (IdentWithShape name' exprs') ->
            error $ "ShapeError: " ++ name 
              ++ ", actual no shape"
              ++ ", expected " ++ show (length exprs')
          otherwise -> SymTable header prmts hashMap
        Nothing -> error $ "InternalError: No AId"
    Nothing -> error $ "SemanticError: Undefined variable " ++ name

aIdentNameWithShape :: String -> [Expr] -> [SymTable] -> SymTable -> SymTable
aIdentNameWithShape name exprs tables (SymTable header prmts hashMap) = 
  case stLookupHashMap name hashMap of
    Just value -> 
      case stAttrId value of
        Just (AId ident) -> case ident of
          (IdentWithShape name' exprs') -> 
            if length exprs == length exprs'
            then SymTable header prmts hashMap 
            else error $ "ShapeError: " ++ name 
              ++ ", actual " ++ show (length exprs)
              ++ ", expected " ++ show (length exprs')
          otherwise -> error $ "ShapeError: " ++ name 
            ++ ", actual " ++ show (length exprs)
            ++ ", expected no shape"
        Nothing -> error $ "InternalError: No AId"
    Nothing -> error $ "SemanticError: Undefined variable " ++ name
  
aCall :: String -> [Expr] -> [SymTable] -> SymTable -> SymTable
aCall name exprs tables table = 
  case stLookupSymTable name tables of
    Just (SymTable ident prmts hashMap) -> 
      if length exprs == length prmts then table
      else error $ "CallError: Incorrect number of arguments for " ++ ident
        ++ ", actual " ++ show (length exprs)
        ++ ", expected " ++ show (length prmts)
    Nothing -> error $ "SemanticError: Undefined procedure " ++ name