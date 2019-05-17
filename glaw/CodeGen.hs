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


module CodeGen where

import GoatAST
import SymTable
import Control.Monad.State

type LabelCounter = Int 

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
--     )

-- getState :: CodeGen State
-- getState = 
--   CodeGen (\st -> (st, st))

type CodeGen a = State LabelCounter a

getLabelCounter :: CodeGen LabelCounter
getLabelCounter =
  do 
    label <- get
    return label

incLabelCounter :: CodeGen ()
incLabelCounter =
  do
    label <- get
    put (label + 1)
    return ()

indentation = "    "
startState = 1
labelCounter = getLabelCounter

codegen :: Program -> [SymTable] -> String
codegen (Program procs) tables = 
  do
    str <- 
      indentation ++ "call proc_main\n"
      ++ indentation ++ "halt"
      ++ (cProcs procs tables)
    return str

cProcs :: [Procedure] -> [SymTable] -> String
cProcs [] _ = ""
cProcs [proc] tables = cProc proc tables
cProcs (proc:procs) tables = cProc proc tables ++ (cProcs procs tables)

cProc :: Procedure -> [SymTable] -> String
cProc (Procedure pos ident prmts decls stmts) tables = 
  case stLookupSymTable ident tables of
    Just table ->
      do
        str <-
          "\nproc_" ++ ident ++ ":\n"
          ++ cPushStackFrame table
          ++ cPrmts prmts table
          ++ cStmts stmts table
          ++ indentation ++ "return"
        return str
    Nothing -> error $ "RuntimeError: Procedure " ++ ident ++ " not found"

cPushStackFrame :: SymTable -> String
cPushStackFrame (SymTable header prmts hashMap) = 
  indentation ++ "push_stack_frame " ++ show (stSize hashMap) ++ "\n"

cPrmts :: [FormalArgSpec] -> SymTable -> String
cPrmts [] _ = ""
cPrmts [prmt] table = cPrmt prmt table
cPrmts (prmt:prmts) table = cPrmt prmt table ++ cPrmts prmts table

cPrmt :: FormalArgSpec -> SymTable -> String
cPrmt (FormalArgSpec pos parMode baseType ident) 
  (SymTable header prmts hashMap) =
  case baseType of
    BoolType -> 
      cBoolConst False 
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = 0
    IntType -> 
      cIntConst 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = 0
    FloatType -> 
      cFloatConst 0.0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = 0
    StringType -> 
      cStringConst ""
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = 0

cIntConst :: Int -> String
cIntConst const = 
  indentation ++ "int_const " ++ "r0, " 
  ++ show const ++ "\n"

cFloatConst :: Float -> String
cFloatConst const = 
  indentation ++ "real_const " ++ "r0, " 
  ++ show const ++ "\n"

cStringConst :: String -> String
cStringConst const = 
  indentation ++ "string_const " ++ "r0, " 
  ++ ('\"' : (const ++ "\"")) ++ "\n"

cBoolConst :: Bool -> String
cBoolConst const = 
  indentation ++ "bool_const " ++ "r0, " ++
  case const of
    True -> "true"
    False -> "false"
  ++ "\n"

cStmts :: [Stmt] -> SymTable -> String
cStmts [] _ = ""
cStmts [stmt] table = cStmt stmt table
cStmts (stmt:stmts) table = 
  cStmt stmt table ++ (cStmts stmts table)

cStmt :: Stmt -> SymTable -> String
cStmt (Write pos expr) table = 
  do
    str <-
      (cExpr expr table)
      ++ indentation ++ "call_builtin print_string\n"
    return str

cExpr :: Expr -> SymTable -> String
cExpr (StrCon pos s) table = 
  do
    str <-
      indentation ++ "string_const "
      ++ "r0" ++ ", " ++ ('\"' : (s ++ "\"")) ++ "\n"
    return str