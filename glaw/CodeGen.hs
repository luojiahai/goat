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
  ++ show (stHashMapSize hashMap) ++ "\n"

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
  cInitBase (Decl pos ident (Base baseType)) table
cDecl (Decl pos ident (Array baseType i)) table = 
  cInitArray (Decl pos ident (Array baseType i)) table
cDecl (Decl pos ident (Matrix baseType i j)) table = 
  cInitMatrix (Decl pos ident (Matrix baseType i j)) table
    
cInitBase :: Decl -> SymTable -> String
cInitBase (Decl pos ident (Base baseType)) table =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table

cInitArray :: Decl -> SymTable -> String
cInitArray (Decl pos ident (Array baseType i)) table =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table

cInitMatrix :: Decl -> SymTable -> String
cInitMatrix (Decl pos ident (Matrix baseType i j)) table =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table

cStore :: Int -> Int -> Int -> String
cStore slot reg 0 = ""
cStore slot reg n = 
  indentation ++ "store " ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
  ++ cStore (slot + 1) reg (n - 1)

cGetSlot :: Ident -> SymTable -> Int
cGetSlot ident (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just symbol ->
      case stASlot symbol of
        Just (ASlot slot) -> slot
        Nothing -> error $ "InternalError: No ASlot"
    Nothing -> error $ "InternalError: cGetSlot " ++ ident

cGetBaseType :: Ident -> SymTable -> BaseType
cGetBaseType ident (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just symbol ->
      case stAType symbol of
        Just (AType baseType) -> baseType
        Nothing -> error $ "InternalError: No AType"
    Nothing -> error $ "InternalError: cGetBaseType " ++ ident

cGetGoatType :: Ident -> SymTable -> GoatType
cGetGoatType ident (SymTable header prmts hashMap) =
  case stLookupHashMap ident hashMap of
    Just symbol ->
      case stAGoatType symbol of
        Just (AGoatType goatType) -> goatType
        Nothing -> error $ "InternalError: No AGoatType"
    Nothing -> error $ "InternalError: cGetGoatType " ++ ident

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
cStmt (Assign pos lvalue expr) table = 
  cAssign (Assign pos lvalue expr) table
cStmt (Read pos lvalue) table = 
  cRead (Read pos lvalue) table
cStmt (Write pos expr) table = 
  cWrite (Write pos expr) table
cStmt (ProcCall pos ident exprs) table = 
  cProcCall (ProcCall pos ident exprs) table
cStmt (If pos expr stmts) table = 
  cIf (If pos expr stmts) table
cStmt (IfElse pos expr stmts1 stmts2) table = 
  cIfElse (IfElse pos expr stmts1 stmts2) table
cStmt (While pos expr stmts) table = 
  cWhile (While pos expr stmts) table

cAssign :: Stmt -> SymTable -> String
cAssign (Assign pos lvalue expr) table =
  cExpr expr 0 table
  ++ cLvalue lvalue 0 table

cRead :: Stmt -> SymTable -> String
cRead (Read pos lvalue) table = 
  cCallBuiltin ("read_" ++ t) table
  ++ cLvalue lvalue 0 table
  where 
    t = 
      case cGetBaseType (cGetLvalueIdent lvalue) table of
        BoolType -> "bool"
        IntType -> "int"
        FloatType -> "real"
        StringType -> error $ "RuntimeError: Cannot read string"

cWrite :: Stmt -> SymTable -> String
cWrite (Write pos expr) table = 
  cExpr expr 0 table
  ++ (cCallBuiltin "print_string" table)

cProcCall :: Stmt -> SymTable -> String
cProcCall (ProcCall pos ident exprs) table = 
  cExprs exprs 0 table
  ++ indentation ++ "call proc_" ++ ident ++ "\n"

cIf :: Stmt -> SymTable -> String
cIf (If pos expr stmts) table = ""

cIfElse :: Stmt -> SymTable -> String
cIfElse (IfElse pos expr stmts1 stmts2) table = ""

cWhile :: Stmt -> SymTable -> String
cWhile (While pos expr stmts) table = ""

cGetLvalueIdent :: Lvalue -> String
cGetLvalueIdent (LId pos ident) = ident
cGetLvalueIdent (LArrayRef pos ident expr) = ident
cGetLvalueIdent (LMatrixRef pos ident expr1 expr2) = ident

cLvalue :: Lvalue -> Int -> SymTable -> String
cLvalue (LId pos ident) reg table = 
  indentation ++ "store " ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = cGetSlot ident table
cLvalue (LArrayRef pos ident expr) reg table = 
  cExpr expr (reg + 2) table
  ++ indentation ++ "load_address " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show (reg + 1) ++ ", "
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 2) ++ "\n"
  ++ indentation ++ "store_indirect " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = cGetSlot ident table
cLvalue (LMatrixRef pos ident expr1 expr2) reg table = 
  cExpr expr1 (reg + 2) table ++ cExpr expr2 (reg + 3) table
  ++ indentation ++ "int_const " ++ "r" ++ show (reg + 4) 
  ++ ", " ++ show len ++ "\n"
  ++ indentation ++ "mul_int " ++ "r" ++ show (reg + 2)
  ++ ", " ++ "r" ++ show (reg + 2) ++ ", " ++ "r" ++ show (reg + 4) ++ "\n"
  ++ indentation ++ "add_int " ++ "r" ++ show (reg + 2)
  ++ ", " ++ "r" ++ show (reg + 2) ++ ", " ++ "r" ++ show (reg + 3) ++ "\n"
  ++ indentation ++ "load_address " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show (reg + 1) ++ ", "
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 2) ++ "\n"
  ++ indentation ++ "store_indirect " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show reg ++ "\n"
  where 
    slot = cGetSlot ident table
    len = 
      case cGetGoatType ident table of
        (Matrix baseType i j) -> i
        otherwise -> error $ "InternalError: Not MatrixRef"

cCallBuiltin :: Ident -> SymTable -> String
cCallBuiltin ident table = indentation ++ "call_builtin " ++ ident ++ "\n"

cExprs :: [Expr] -> Int -> SymTable -> String
cExprs [] _ _ = ""
cExprs [expr] reg table = cExpr expr reg table
cExprs (expr:exprs) reg table = 
  cExpr expr reg table ++ cExprs exprs (reg + 1) table

cId :: Expr -> Int -> SymTable -> String
cId (Id pos ident) reg table =
  indentation ++ "load " ++ "r" ++ show reg ++ ", " ++ show slot ++ "\n"
  where slot = cGetSlot ident table

cArrayRef :: Expr -> Int -> SymTable -> String
cArrayRef (ArrayRef pos ident expr) reg table =
  cExpr expr (reg + 1) table
  ++ indentation ++ "load_address " 
  ++ "r" ++ show reg ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show reg ++ ", "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"
  ++ indentation ++ "load_indirect " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = cGetSlot ident table

cMatrixRef :: Expr -> Int -> SymTable -> String
cMatrixRef (MatrixRef pos ident expr1 expr2) reg table = 
  cExpr expr1 (reg + 1) table ++ cExpr expr2 (reg + 2) table
  ++ indentation ++ "int_const " ++ "r" ++ show (reg + 3) 
  ++ ", " ++ show len ++ "\n"
  ++ indentation ++ "mul_int " ++ "r" ++ show (reg + 1)
  ++ ", " ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 3) ++ "\n"
  ++ indentation ++ "add_int " ++ "r" ++ show (reg + 1)
  ++ ", " ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 2) ++ "\n"
  ++ indentation ++ "load_address " 
  ++ "r" ++ show reg ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show reg ++ ", "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"
  ++ indentation ++ "load_indirect " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where 
    slot = cGetSlot ident table
    len = 
      case cGetGoatType ident table of
        (Matrix baseType i j) -> i
        otherwise -> error $ "InternalError: Not MatrixRef"

cAnd :: Expr -> Int -> SymTable -> String
cAnd (And pos expr1 expr2) reg table =
  cExpr expr1 reg table
  ++ (cExpr expr2 (reg + 1) table)
  ++ indentation ++ "and " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

cOr :: Expr -> Int -> SymTable -> String
cOr (Or pos expr1 expr2) reg table =
  cExpr expr1 reg table
  ++ (cExpr expr2 (reg + 1) table)
  ++ indentation ++ "or " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

cNot :: Expr -> Int -> SymTable -> String
cNot (Not pos expr) reg table =
  cExpr expr reg table
  ++ indentation ++ "not " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ "\n"

cRel :: Expr -> Int -> SymTable -> String
cRel (Rel pos relOp expr1 expr2) reg table =
  cExpr expr1 reg table
  ++ (cExpr expr2 (reg + 1) table)
  ++ (cConvertType expr1 expr2 (reg + 1) table)
  ++ indentation ++ "cmp_" ++ op ++ "_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show (reg + 1) ++ "\n"
  where 
    op = case relOp of
           Op_eq -> "eq"
           Op_ne -> "ne"
           Op_ge -> "ge"
           Op_le -> "le"
           Op_gt -> "gt"
           Op_lt -> "lt"
    t = case cGetExprBaseType expr1 table of
          IntType -> "int"
          FloatType -> "real"

cBinOpExp :: Expr -> Int -> SymTable -> String
cBinOpExp (BinOpExp pos binOp expr1 expr2) reg table =
  cCheckTypeNum expr1 table ++ (cExpr expr1 reg table)
  ++ (cExpr expr2 (reg + 1) table)
  ++ (cConvertType expr1 expr2 (reg + 1) table)
  ++ indentation ++ op ++ "_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show (reg + 1) ++ "\n"
  where 
    op = case binOp of
           Op_add -> "add"
           Op_sub -> "sub"
           Op_mul -> "mul"
           Op_div -> "div"
    t = case cGetExprBaseType expr1 table of
          IntType -> "int"
          FloatType -> "real"

cUnaryMinus :: Expr -> Int -> SymTable -> String
cUnaryMinus (UnaryMinus pos expr) reg table =
  cCheckTypeNum expr table ++ (cExpr expr reg table)
  ++ indentation ++ "neg_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show (reg + 1) ++ "\n"
  where 
    t = case cGetExprBaseType expr table of
          IntType -> "int"
          FloatType -> "real"

cCheckTypeNum :: Expr -> SymTable -> String
cCheckTypeNum expr table =
  case cGetExprBaseType expr table of
    IntType -> ""
    FloatType -> ""
    otherwise -> error $ "RuntimeError: Type error"
  
cConvertType :: Expr -> Expr -> Int -> SymTable -> String       
cConvertType expr1 expr2 reg table =
  case cGetExprBaseType expr1 table of
    IntType -> ""
    FloatType -> 
      case cGetExprBaseType expr2 table of
        FloatType -> ""
        IntType ->
          indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        otherwise -> error $ "RuntimeError: Type error"
    otherwise -> error $ "RuntimeError: Type error"

cExpr :: Expr -> Int -> SymTable -> String
cExpr (BoolCon pos const) reg table = cBoolConst const reg
cExpr (IntCon pos const) reg table = cIntConst const reg
cExpr (FloatCon pos const) reg table = cFloatConst const reg
cExpr (StrCon pos const) reg table = cStringConst const reg
cExpr (Id pos ident) reg table = cId (Id pos ident) reg table
cExpr (ArrayRef pos ident expr) reg table = 
  cArrayRef (ArrayRef pos ident expr) reg table 
cExpr (MatrixRef pos ident expr1 expr2) reg table =
  cMatrixRef (MatrixRef pos ident expr1 expr2) reg table
cExpr (And pos expr1 expr2) reg table = 
  cAnd (And pos expr1 expr2) reg table
cExpr (Or pos expr1 expr2) reg table = 
  cOr (Or pos expr1 expr2) reg table
cExpr (Not pos expr) reg table = 
  cNot (Not pos expr) reg table
cExpr (Rel pos relOp expr1 expr2) reg table = 
  cRel (Rel pos relOp expr1 expr2) reg table
cExpr (BinOpExp pos binOp expr1 expr2) reg table = 
  cBinOpExp (BinOpExp pos binOp expr1 expr2) reg table
cExpr (UnaryMinus pos expr) reg table = 
  cUnaryMinus (UnaryMinus pos expr) reg table

cGetExprBaseType :: Expr -> SymTable -> BaseType
cGetExprBaseType (BoolCon pos const) table = BoolType
cGetExprBaseType (IntCon pos const) table = IntType
cGetExprBaseType (FloatCon pos const) table = FloatType
cGetExprBaseType (StrCon pos const) table = StringType
cGetExprBaseType (Id pos ident) table = 
  cGetBaseType ident table
cGetExprBaseType (ArrayRef pos ident expr) table = 
  cGetBaseType ident table
cGetExprBaseType (MatrixRef pos ident expr1 expr2) table = 
  cGetBaseType ident table
cGetExprBaseType (And pos expr1 expr2) table = BoolType
cGetExprBaseType (Or pos expr1 expr2) table = BoolType
cGetExprBaseType (Not pos expr) table = BoolType
cGetExprBaseType (Rel pos relOp expr1 expr2) table = BoolType
cGetExprBaseType (BinOpExp pos binOp expr1 expr2) table = 
  cGetExprBaseType expr1 table
cGetExprBaseType (UnaryMinus pos expr) table = 
  cGetExprBaseType expr table