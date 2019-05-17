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
          ++ cStackFrame "push_stack_frame" table
          ++ cPrmts prmts table
          ++ cDecls decls table
          ++ cStmts stmts table
          ++ cStackFrame "pop_stack_frame" table
          ++ indentation ++ "return"
        return str
    Nothing -> error $ "RuntimeError: Procedure " ++ ident ++ " not found"

cStackFrame :: String -> SymTable -> String
cStackFrame command (SymTable header prmts hashMap) = 
  indentation ++ command ++ " " ++ show (stSize hashMap) ++ "\n"

cPrmts :: [FormalArgSpec] -> SymTable -> String
cPrmts [] _ = ""
cPrmts [prmt] table = cPrmt prmt table
cPrmts (prmt:prmts) table = cPrmt prmt table ++ cPrmts prmts table

cPrmt :: FormalArgSpec -> SymTable -> String
cPrmt (FormalArgSpec pos parMode baseType ident) 
  (SymTable header prmts hashMap) =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    IntType -> 
      cIntConst 0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    FloatType -> 
      cFloatConst 0.0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    StringType -> 
      cStringConst "" 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap

cDecls :: [Decl] -> SymTable -> String
cDecls [] _ = ""
cDecls [decl] table = cDecl decl table
cDecls (decl:decls) table = cDecl decl table ++ cDecls decls table

cDecl :: Decl -> SymTable -> String
cDecl (Decl pos ident (Base baseType)) 
  (SymTable header prmts hashMap) =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    IntType -> 
      cIntConst 0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    FloatType -> 
      cFloatConst 0.0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap
    StringType -> 
      cStringConst "" 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident hashMap

cGetSlot :: Ident -> HashMap -> Int
cGetSlot ident hashMap =
  case stLookupHashMap ident hashMap of
    Just symbol ->
      case stASlot symbol of
        Just (ASlot slot) -> slot
        Nothing -> error $ "InternalError: No ASlot"
    Nothing -> error $ "InternalError: cGetSlot " ++ ident

cIntConst :: Int -> Int -> String
cIntConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

cFloatConst :: Float -> Int -> String
cFloatConst const reg = 
  indentation ++ "real_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

cStringConst :: String -> Int -> String
cStringConst const reg = 
  indentation ++ "string_const " ++ "r" ++ show reg
  ++ ", " ++ ('\"' : (const ++ "\"")) ++ "\n"

cBoolConst :: Bool -> Int -> String
cBoolConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg 
  ++ ", " ++
  case const of
    True -> show 1
    False -> show 0
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
      (cExpr expr 0 table)
      ++ indentation ++ "call_builtin print_string\n"
    return str
cStmt (ProcCall pos ident exprs) table = 
  do
    str <-
      indentation ++ "call "
      ++ "proc_" ++ ident ++ "\n"
    return str

cExpr :: Expr -> Int -> SymTable -> String
cExpr (BoolCon pos const) reg table = cBoolConst const reg
cExpr (IntCon pos const) reg table = cIntConst const reg
cExpr (FloatCon pos const) reg table = cFloatConst const reg
cExpr (StrCon pos const) reg table = cStringConst const reg
-- cExpr (Id pos ident) =
-- cExpr (ArrayRef pos ident expr) =
-- cExpr (MatrixRef Pos Ident expr expr) =