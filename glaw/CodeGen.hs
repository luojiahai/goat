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

-- evalState (incLabelCounter >> labelCounter) startState

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
          ++ cStackFrame "push" table
          ++ cPrmts prmts table
          ++ cDecls decls table
          ++ cStmts stmts table
          ++ cStackFrame "pop" table
          ++ indentation ++ "return"
        return str
    Nothing -> error $ "RuntimeError: Procedure " ++ ident ++ " not found"

cStackFrame :: String -> SymTable -> String
cStackFrame command (SymTable header prmts hashMap) = 
  indentation ++ command ++ "_stack_frame" ++ " " 
  ++ show (stSize hashMap) ++ "\n"

cPrmts :: [FormalArgSpec] -> SymTable -> String
cPrmts [] _ = ""
cPrmts [prmt] table = cPrmt prmt table
cPrmts (prmt:prmts) table = cPrmt prmt table ++ cPrmts prmts table

cPrmt :: FormalArgSpec -> SymTable -> String
cPrmt (FormalArgSpec pos parMode baseType ident) table =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident table
    IntType -> 
      cIntConst 0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident table
    FloatType -> 
      cFloatConst 0.0 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident table
    StringType -> 
      cStringConst "" 0
      ++ indentation ++ "store " ++ show slot ++ ", " ++ "r0\n"
      where slot = cGetSlot ident table

cDecls :: [Decl] -> SymTable -> String
cDecls [] _ = ""
cDecls [decl] table = cDecl decl table
cDecls (decl:decls) table = cDecl decl table ++ cDecls decls table

cDecl :: Decl -> SymTable -> String
cDecl (Decl pos ident (Base baseType)) table =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0
      where slot = cGetSlot ident table
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0
      where slot = cGetSlot ident table
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0
      where slot = cGetSlot ident table
    StringType -> 
      cStringConst "" 0
      ++ cStore slot 0
      where slot = cGetSlot ident table
    
cStore :: Int -> Int -> String
cStore slot reg = 
  indentation ++ "store " ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"

cGetSlot :: Ident -> SymTable -> Int
cGetSlot ident (SymTable header prmts hashMap) =
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
cStmt (Assign pos lvalue expr) table = ""
cStmt (Read pos lvalue) table = ""
cStmt (Write pos expr) table = 
  do
    str <-
      (cExpr expr 0 table)
      ++ (cCallBuiltin "print_string" table)
    return str
cStmt (ProcCall pos ident exprs) table = 
  do
    str <-
      (cCallArgs exprs 0 table)
      ++ (cCallProc ident table)
    return str
cStmt (If pos expr stmts) table = ""
cStmt (IfElse pos expr stmts1 stmts2) table = ""
cStmt (While pos expr stmts) table = ""

cCallProc :: Ident -> SymTable -> String
cCallProc ident table = indentation ++ "call proc_" ++ ident ++ "\n"

cCallBuiltin :: Ident -> SymTable -> String
cCallBuiltin ident table = indentation ++ "call_builtin " ++ ident ++ "\n"

cCallArgs :: [Expr] -> Int -> SymTable -> String
cCallArgs [] _ _ = ""
cCallArgs [expr] reg table = cCallArg expr reg table
cCallArgs (expr:exprs) reg table = 
  cCallArg expr reg table ++ cCallArgs exprs (reg+1) table

cCallArg :: Expr -> Int -> SymTable -> String
cCallArg expr reg table = cExpr expr reg table

cId :: Ident -> Int -> SymTable -> String
cId ident reg table =
  indentation ++ "load " ++ "r" ++ show reg
  ++ ", " ++ show slot ++ "\n"
  where slot = cGetSlot ident table

cArrayRef :: Ident -> Int -> Int -> SymTable -> String
cArrayRef ident i reg table = ""

cMatrixRef :: Ident -> Int -> Int -> Int -> SymTable -> String
cMatrixRef ident i j reg table = ""

cExpr :: Expr -> Int -> SymTable -> String
cExpr (BoolCon pos const) reg table = cBoolConst const reg
cExpr (IntCon pos const) reg table = cIntConst const reg
cExpr (FloatCon pos const) reg table = cFloatConst const reg
cExpr (StrCon pos const) reg table = cStringConst const reg
cExpr (Id pos ident) reg table = cId ident reg table
cExpr (ArrayRef pos ident expr) reg table = ""
cExpr (MatrixRef pos ident expr1 expr2) reg table = ""
cExpr (And pos expr1 expr2) reg table = ""
cExpr (Or pos expr1 expr2) reg table = ""
cExpr (Not pos expr) reg table = ""
cExpr (Rel pos relOp expr1 expr2) reg table = ""
cExpr (BinOpExp pos binOp expr1 expr2) reg table = ""
cExpr (UnaryMinus pos expr) reg table = ""