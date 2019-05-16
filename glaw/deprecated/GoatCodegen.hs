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


module GoatCodegen where

import GoatAST
import GoatSymTable

-- type LabelCounter = Int

-- type Env = String

-- data State = State Env LabelCounter

-- data CodeGen a = CodeGen (State -> (a, State))

-- instance Monad CodeGen where
--   return code =
--     CodeGen (\st -> (code, st))
--   CodeGen gen >>= f =
--     CodeGen (\st0 -> 
--       let
--         (code', st1) = gen st0
--         CodeGen gen' = f code'
--       in gen' st1
--       )

-- getState :: CodeGen State
-- getState = 
--   CodeGen (\st -> (st, st))

-- getLabelCounter :: CodeGen LabelCounter 
-- getLabelCounter =
--   do 
--     State _ lc <- getState
--     return lc

-- incLabelCounter :: CodeGen ()
-- incLabelCounter =
--   CodeGen (\(State env lc) -> ((), State env (lc + 1)))


indentation = "    "

codegen :: GoatProgram -> [SymTable] -> String
codegen (GoatProgram procs) tables = 
  do
    str <- 
      indentation ++ "call main\n"
      ++ indentation ++ "halt\n"
      ++ (cProcs procs tables)
    return str

cProcs :: [Procedure] -> [SymTable] -> String
cProcs [] _ = ""
cProcs [proc] tables = cProc proc tables
cProcs (proc:procs) tables = cProc proc tables ++ (cProcs procs tables)

cProc :: Procedure -> [SymTable] -> String
cProc (Procedure ident prmts decls stmts) tables = 
  case stLookupSymTable ident tables of
    Just table ->
      do
        str <-
          ident ++ ":\n"
          ++ cStmts stmts table
          ++ indentation ++ "return"
        return str
    Nothing -> error $ "RuntimeError: Procedure " ++ ident ++ " not found"

cStmts :: [Stmt] -> SymTable -> String
cStmts [] _ = ""
cStmts [stmt] table = cStmt stmt table
cStmts (stmt:stmts) table = 
  cStmt stmt table ++ (cStmts stmts table)

cStmt :: Stmt -> SymTable -> String
cStmt (Read lvalue) table = 
  do
    str <-
      indentation ++ "call_builtin read\n"
    return str
cStmt (Write expr) table = 
  do
    str <-
      (cExpr expr table)
      ++ indentation ++ "call_builtin print_string\n"
    return str

cExpr :: Expr -> SymTable -> String
cExpr (StrConst s) table = 
  do
    str <-
      indentation ++ "string_const "
      ++ "r0" ++ ", " ++ (show s) ++ "\n"
    return str