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

indentation = "    "


-- generate oz code of a goat program
codegen :: Program -> [SymTable] -> String
codegen (Program procs) tables = 
  indentation ++ "call proc_main\n"
  ++ indentation ++ "halt"
  ++ procsStr
  where (_, procsStr) = cProcs procs 0 tables

-- generate oz code of a sequence of procedures
cProcs :: [Procedure] -> Int -> [SymTable] -> (Int, String)
cProcs [] label _ = (label, "")
cProcs [proc] label tables = 
  let (label', str) = cProc proc label tables 
  in (label', str)
cProcs (proc:procs) label tables = 
  let 
    (label', str) = cProc proc label tables 
    (label'', str') = cProcs procs label' tables
  in (label'', str ++ str')

-- generate oz code of a procedure
cProc :: Procedure -> Int -> [SymTable] -> (Int, String)
cProc (Procedure pos ident prmts decls stmts) label tables = 
  case stLookupSymTable ident tables of
    Just table ->
      (label', "\nproc_" ++ ident ++ ":\n"
      ++ cStackFrame "push" table tables
      ++ cPrmts prmts 0 table tables
      ++ cDecls decls table tables
      ++ stmtsStr
      ++ cStackFrame "pop" table tables
      ++ indentation ++ "return")
      where (label', stmtsStr) = cStmts stmts label table tables
    Nothing -> 
      error $ "RuntimeError: Procedure " 
      ++ ident ++ " not found " ++ show pos

-- generate oz code of push/pull stack frame
cStackFrame :: String -> SymTable -> [SymTable] -> String
cStackFrame command (SymTable header prmts hashMap) tables = 
  indentation ++ command ++ "_stack_frame" ++ " " 
  ++ show (stHashMapSize hashMap) ++ "\n"

-- generate oz code of a sequence of parameters
cPrmts :: [FormalArgSpec] -> Int -> SymTable -> [SymTable] -> String
cPrmts [] _ _ _ = ""
cPrmts [prmt] reg table tables = cPrmt prmt reg table tables
cPrmts (prmt:prmts) reg table tables = 
  cPrmt prmt reg table tables ++ cPrmts prmts (reg + 1) table tables

-- generate oz code of a parameter
cPrmt :: FormalArgSpec -> Int -> SymTable -> [SymTable] -> String
cPrmt (FormalArgSpec pos parMode baseType ident) reg table tables =
  case baseType of
    BoolType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = cGetSlot ident table tables
    IntType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = cGetSlot ident table tables
    FloatType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = cGetSlot ident table tables

-- generate oz code of a sequence of declarations
cDecls :: [Decl] -> SymTable -> [SymTable] -> String
cDecls [] _ _ = ""
cDecls [decl] table tables = cDecl decl table tables
cDecls (decl:decls) table tables = 
  cDecl decl table tables ++ cDecls decls table tables

-- generate oz code of a declaration
cDecl :: Decl -> SymTable -> [SymTable] -> String
cDecl (Decl pos ident (Base baseType)) table tables =
  cInitBase (Decl pos ident (Base baseType)) table tables
cDecl (Decl pos ident (Array baseType i)) table tables = 
  cInitArray (Decl pos ident (Array baseType i)) table tables
cDecl (Decl pos ident (Matrix baseType i j)) table tables = 
  cInitMatrix (Decl pos ident (Matrix baseType i j)) table tables
    
-- generate oz code of initialization of base id
cInitBase :: Decl -> SymTable -> [SymTable] -> String
cInitBase (Decl pos ident (Base baseType)) table tables =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table tables
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table tables
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 1
      where slot = cGetSlot ident table tables

-- generate oz code of initialization of array id
cInitArray :: Decl -> SymTable -> [SymTable] -> String
cInitArray (Decl pos ident (Array baseType i)) table tables =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table tables
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table tables
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 i
      where slot = cGetSlot ident table tables

-- generate oz code of initialization of matrix id
cInitMatrix :: Decl -> SymTable -> [SymTable] -> String
cInitMatrix (Decl pos ident (Matrix baseType i j)) table tables =
  case baseType of
    BoolType -> 
      cBoolConst False 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table tables
    IntType -> 
      cIntConst 0 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table tables
    FloatType -> 
      cFloatConst 0.0 0
      ++ cStore slot 0 (i * j)
      where slot = cGetSlot ident table tables

-- generate oz code of store action
cStore :: Int -> Int -> Int -> String
cStore slot reg 0 = ""
cStore slot reg n = 
  indentation ++ "store " 
  ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
  ++ cStore (slot + 1) reg (n - 1)

-- get slot number given an id
cGetSlot :: Ident -> SymTable -> [SymTable] -> Int
cGetSlot ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stASlot symbol of
        Just (ASlot slot) -> slot
        Nothing -> error $ "InternalError: cGetSlot"
    Nothing -> error $ "InternalError: cGetSlot " ++ ident

-- get base type given an id
cGetBaseType :: Ident -> SymTable -> [SymTable] -> BaseType
cGetBaseType ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stAType symbol of
        Just (AType baseType) -> baseType
        Nothing -> error $ "InternalError: cGetBaseType"
    Nothing -> error $ "InternalError: cGetBaseType " ++ ident

-- get goat type given an id
cGetGoatType :: Ident -> SymTable -> [SymTable] -> GoatType
cGetGoatType ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stAGoatType symbol of
        Just (AGoatType goatType) -> goatType
        Nothing -> error $ "InternalError: cGetGoatType"
    Nothing -> error $ "InternalError: cGetGoatType " ++ ident

-- generate oz code of int constant
cIntConst :: Int -> Int -> String
cIntConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

-- generate oz code of float constant
cFloatConst :: Float -> Int -> String
cFloatConst const reg = 
  indentation ++ "real_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

-- generate oz code of string constant
cStringConst :: String -> Int -> String
cStringConst const reg = 
  indentation ++ "string_const " ++ "r" ++ show reg
  ++ ", " ++ ('\"' : (const ++ "\"")) ++ "\n"

-- generate oz code of bool constant
cBoolConst :: Bool -> Int -> String
cBoolConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg 
  ++ ", " ++
  case const of
    True -> show 1
    False -> show 0
  ++ "\n"

-- generate oz code of a sequence of statements
cStmts :: [Stmt] -> Int -> SymTable -> [SymTable] -> (Int, String)
cStmts [] label _ _ = (label, "")
cStmts [stmt] label table tables = cStmt stmt label table tables
cStmts (stmt:stmts) label table tables = 
  let 
    (label', str) = cStmt stmt label table tables 
    (label'', str') = cStmts stmts label' table tables
  in (label'', str ++ str')

-- generate oz code of a statement
cStmt :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
cStmt (Assign pos lvalue expr) label table tables = 
  (label, cAssign (Assign pos lvalue expr) table tables)
cStmt (Read pos lvalue) label table tables = 
  (label, cRead (Read pos lvalue) table tables)
cStmt (Write pos expr) label table tables = 
  (label, cWrite (Write pos expr) table tables)
cStmt (ProcCall pos ident exprs) label table tables = 
  (label, cProcCall (ProcCall pos ident exprs) table tables)
cStmt (If pos expr stmts) label table tables = 
  cIf (If pos expr stmts) label table tables
cStmt (IfElse pos expr stmts1 stmts2) label table tables = 
  cIfElse (IfElse pos expr stmts1 stmts2) label table tables
cStmt (While pos expr stmts) label table tables = 
  cWhile (While pos expr stmts) label table tables

-- generate oz code of assignment statement
cAssign :: Stmt -> SymTable -> [SymTable] -> String
cAssign (Assign pos lvalue expr) table tables =
  cExpr expr 0 table tables
  ++
  case cGetBaseType (cGetLvalueIdent lvalue) table tables of
    FloatType -> 
      case cGetExprBaseType expr table tables of
        IntType -> 
          indentation ++ "int_to_real " 
          ++ "r" ++ show 0 ++ ", " ++ "r" ++ show 0 ++ "\n"
        otherwise -> ""
    otherwise -> ""
  ++ cLvalue lvalue 0 table tables

-- generate oz code of read statement
cRead :: Stmt -> SymTable -> [SymTable] -> String
cRead (Read pos lvalue) table tables = 
  cCallBuiltin ("read_" ++ t) table tables
  ++ cLvalue lvalue 0 table tables
  where 
    t = 
      case cGetBaseType (cGetLvalueIdent lvalue) table tables of
        BoolType -> "bool"
        IntType -> "int"
        FloatType -> "real"
        StringType -> 
          error $ "RuntimeError: Cannot read string " ++ show pos

-- generate oz code of write statement
cWrite :: Stmt -> SymTable -> [SymTable] -> String
cWrite (Write pos expr) table tables = 
  cExpr expr 0 table tables
  ++ (cCallBuiltin ("print_" ++ t) table tables)
  where 
    t =
      case cGetExprBaseType expr table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> "bool"
        StringType -> "string"

-- generate oz code of procedure call statement
cProcCall :: Stmt -> SymTable -> [SymTable] -> String
cProcCall (ProcCall pos ident exprs) table tables = 
  cProcArgs ident exprs 0 0 table tables
  ++ indentation ++ "call proc_" ++ ident ++ "\n"

-- generate oz code of if statement
cIf :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
cIf (If pos expr stmts) label table tables = 
  (label', cExpr expr 0 table tables
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show label ++ "\n"
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ "label_" ++ show label ++ ":\n" ++ stmtsStr
  ++ "label_" ++ show (label + 1) ++ ":\n")
  where (label', stmtsStr) = cStmts stmts (label + 2) table tables

-- generate oz code of if-else statement
cIfElse :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
cIfElse (IfElse pos expr stmts1 stmts2) label table tables = 
  (label'', cExpr expr 0 table tables
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show label ++ "\n"
  ++ indentation ++ "branch_on_false " 
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ "label_" ++ show label ++ ":\n" ++ stmts1Str
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 2) ++ "\n"
  ++ "label_" ++ show (label + 1) ++ ":\n" ++ stmts2Str
  ++ "label_" ++ show (label + 2) ++ ":\n")
  where 
    (label', stmts1Str) = cStmts stmts1 (label + 3) table tables
    (label'', stmts2Str) = cStmts stmts2 label' table tables

-- generate oz code of while statement
cWhile :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
cWhile (While pos expr stmts) label table tables = 
  (label', "label_" ++ show label ++ ":\n"
  ++ (cExpr expr 1 table tables)
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 1 ++ ", " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 2) ++ "\n"
  ++ "label_" ++ show (label + 1) ++ ":\n" ++ stmtsStr
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show label ++ "\n"
  ++ "label_" ++ show (label + 2)  ++ ":\n")
  where (label', stmtsStr) = cStmts stmts (label + 3) table tables

-- get identifier name of lvalue
cGetLvalueIdent :: Lvalue -> String
cGetLvalueIdent (LId pos ident) = ident
cGetLvalueIdent (LArrayRef pos ident expr) = ident
cGetLvalueIdent (LMatrixRef pos ident expr1 expr2) = ident

-- generate oz code of storing lvalue
cLvalue :: Lvalue -> Int -> SymTable -> [SymTable] -> String
cLvalue (LId pos ident) reg table tables = 
  case stLookupHashMap ident table of
    Just symbol ->
      case stAParMode symbol of
        Just (AParMode Val) -> 
          indentation ++ "store " 
          ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
        Just (AParMode Ref) -> 
          indentation ++ "load " 
          ++ "r" ++ show (reg + 1) ++ ", " ++ show slot ++ "\n"
          ++ indentation ++ "store_indirect " 
          ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show reg ++ "\n"
        Nothing -> 
          indentation ++ "store " 
          ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
    Nothing -> 
      error $ "InternalError: cLvalue " ++ ident ++ " " ++ show pos
  where slot = cGetSlot ident table tables
cLvalue (LArrayRef pos ident expr) reg table tables = 
  cExpr expr (reg + 2) table tables
  ++ indentation ++ "load_address " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show (reg + 1) ++ ", "
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 2) ++ "\n"
  ++ indentation ++ "store_indirect " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = cGetSlot ident table tables
cLvalue (LMatrixRef pos ident expr1 expr2) reg table tables = 
  cExpr expr1 (reg + 2) table tables ++ cExpr expr2 (reg + 3) table tables
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
    slot = cGetSlot ident table tables
    len = 
      case cGetGoatType ident table tables of
        (Matrix baseType i j) -> j
        otherwise -> error $ "InternalError: cLvalue " ++ show pos

-- generate oz code of builtin function call
cCallBuiltin :: Ident -> SymTable -> [SymTable] -> String
cCallBuiltin ident table tables = 
  indentation ++ "call_builtin " ++ ident ++ "\n"

-- generate oz code of a sequence of procedure arguments
cProcArgs :: Ident -> [Expr] -> Int -> Int 
  -> SymTable -> [SymTable] -> String
cProcArgs _ [] _ _ _ _ = ""
cProcArgs ident [expr] i reg table tables = 
  cProcArg ident expr i reg table tables
cProcArgs ident (expr:exprs) i reg table tables = 
  cProcArg ident expr i reg table tables 
  ++ cProcArgs ident exprs (i + 1) (reg + 1) table tables

-- generate oz code of a procedure argument
cProcArg :: Ident -> Expr -> Int -> Int 
  -> SymTable -> [SymTable] -> String
cProcArg ident expr i reg table tables = 
  case stAParMode symbol of
    Just (AParMode parMode) ->
      case parMode of
        Val -> 
          case stAType symbol of
            Just (AType baseType) -> 
              cProcArgVal ident baseType expr reg table tables
            Nothing -> error $ "InternalError: cProcArg"
        Ref -> 
          case stAType symbol of
            Just (AType baseType) -> 
              cProcArgRef ident baseType expr reg table tables
            Nothing -> error $ "InternalError: cProcArg"
    Nothing -> error $ "InternalError: cProcArg"
    where 
      symbol = 
        case stLookupSymTable ident tables of
          Just table' -> stGetArgSymbol i table'
          Nothing -> error $ "InternalError: cProcArg"
  
-- generate oz code of a procedure argument with parmode val
cProcArgVal :: Ident -> BaseType -> Expr -> Int 
  -> SymTable -> [SymTable] -> String
cProcArgVal ident baseType expr reg table tables =
  if (cGetExprBaseType expr table tables) == baseType 
  then cExpr expr reg table tables
  else 
    case baseType of
      FloatType ->
        if (cGetExprBaseType expr table tables) == IntType
        then 
          cExpr expr reg table tables
          ++ indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        else
          error $ "RuntimeError: Type error"
      otherwise -> error $ "RuntimeError: Type error"

-- generate oz code of a procedure argument with parmode ref
cProcArgRef :: Ident -> BaseType -> Expr -> Int 
  -> SymTable -> [SymTable] -> String
cProcArgRef ident baseType expr reg table tables =
  if (cGetExprBaseType expr table tables) == baseType 
  then cExprAddr expr reg table tables
  else
    case baseType of
      FloatType ->
        if (cGetExprBaseType expr table tables) == IntType
        then 
          cExprAddr expr reg table tables
          ++ indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        else
          error $ "RuntimeError: Type error"
      otherwise -> error $ "RuntimeError: Type error"

-- generate oz code of an expression as ref
cExprAddr :: Expr -> Int -> SymTable -> [SymTable] -> String
cExprAddr (Id pos ident) reg table tables = 
  cId (Id pos ident) True reg table tables
cExprAddr (ArrayRef pos ident expr) reg table tables = 
  cArrayRef (ArrayRef pos ident expr) True reg table  tables
cExprAddr (MatrixRef pos ident expr1 expr2) reg table tables =
  cMatrixRef (MatrixRef pos ident expr1 expr2) True reg table tables
cExprAddr _ _ _ _ = 
  error $ "RuntimeError: Ref must be Id/ArrayRef/MatrixRef"

-- generate oz code of loading id
cId :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
cId (Id pos ident) isLoadAddr reg table tables =
  indentation ++ 
  case isLoadAddr of
    True -> 
      "load_address " 
    False -> 
      "load " 
  ++ "r" ++ show reg ++ ", " ++ show slot ++ "\n"
  ++
  case stLookupHashMap ident table of
    Just symbol ->
      case stAParMode symbol of
        Just (AParMode Val) -> ""
        Just (AParMode Ref) -> 
          indentation ++ "load_indirect " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        Nothing -> ""
    Nothing -> 
      error $ "InternalError: cId " ++ ident ++ " " ++ show pos
  where slot = cGetSlot ident table tables

-- generate oz code of loading array reference
cArrayRef :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
cArrayRef (ArrayRef pos ident expr) isLoadAddr reg table tables =
  cExpr expr (reg + 1) table tables
  ++ indentation ++ "load_address " 
  ++ "r" ++ show reg ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show reg ++ ", "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"
  ++
  case isLoadAddr of
    True -> ""
    False ->
      indentation ++ "load_indirect " 
      ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = cGetSlot ident table tables

-- generate oz code of loading matrix reference
cMatrixRef :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
cMatrixRef (MatrixRef pos ident expr1 expr2) isLoadAddr reg table tables = 
  cExpr expr1 (reg + 1) table tables ++ cExpr expr2 (reg + 2) table tables
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
  ++
  case isLoadAddr of
    True -> ""
    False ->
      indentation ++ "load_indirect " 
      ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where 
    slot = cGetSlot ident table tables
    len = 
      case cGetGoatType ident table tables of
        (Matrix baseType i j) -> j
        otherwise -> error $ "InternalError: cMatrixRef " ++ show pos

-- generate oz code of and expression
cAnd :: Expr -> Int -> SymTable -> [SymTable] -> String
cAnd (And pos expr1 expr2) reg table tables =
  cExpr expr1 reg table tables
  ++ (cExpr expr2 (reg + 1) table tables)
  ++ indentation ++ "and " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

-- generate oz code of or expression
cOr :: Expr -> Int -> SymTable -> [SymTable] -> String
cOr (Or pos expr1 expr2) reg table tables =
  cExpr expr1 reg table tables
  ++ (cExpr expr2 (reg + 1) table tables)
  ++ indentation ++ "or " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

-- generate oz code of not expression
cNot :: Expr -> Int -> SymTable -> [SymTable] -> String
cNot (Not pos expr) reg table tables =
  cExpr expr reg table tables
  ++ indentation ++ "not " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ "\n"

-- generate oz code of rel expression
cRel :: Expr -> Int -> SymTable -> [SymTable] -> String
cRel (Rel pos relOp expr1 expr2) reg table tables =
  cExpr expr1 reg table tables
  ++ (cExpr expr2 (reg + 1) table tables)
  ++ (cConvertType expr1 expr2 reg table tables)
  ++ indentation ++ "cmp_" ++ op ++ "_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show (reg + 1) ++ "\n"
  where 
    op = 
      case relOp of
        Op_eq -> "eq"
        Op_ne -> "ne"
        Op_ge -> "ge"
        Op_le -> "le"
        Op_gt -> "gt"
        Op_lt -> "lt"
    t = 
      case cGetExprBaseType expr1 table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> "int"
        StringType -> 
          error $ "RuntimeError: Cannot compare string " ++ show pos

-- generate oz code of binary operation expression
cBinOpExp :: Expr -> Int -> SymTable -> [SymTable] -> String
cBinOpExp (BinOpExp pos binOp expr1 expr2) reg table tables =
  cCheckTypeNum expr1 table tables 
  ++ (cExpr expr1 reg table tables) ++ (cExpr expr2 (reg + 1) table tables)
  ++ (cConvertType expr1 expr2 reg table tables)
  ++ indentation ++ op ++ "_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show (reg + 1) ++ "\n"
  where 
    op = 
      case binOp of
        Op_add -> "add"
        Op_sub -> "sub"
        Op_mul -> "mul"
        Op_div -> "div"
    t = 
      case cGetExprBaseType expr1 table tables of
        IntType -> 
          case cGetExprBaseType expr2 table tables of
            FloatType -> "real"
            IntType -> "int"
            BoolType -> 
              error $ "RuntimeError: Cannot evaluate bool " ++ show pos
            StringType -> 
              error $ "RuntimeError: Cannot evaluate string " ++ show pos
        FloatType -> "real"
        BoolType -> 
          error $ "RuntimeError: Cannot evaluate bool " ++ show pos
        StringType -> 
          error $ "RuntimeError: Cannot evaluate string " ++ show pos

-- generate oz code of unary minus expression
cUnaryMinus :: Expr -> Int -> SymTable -> [SymTable] -> String
cUnaryMinus (UnaryMinus pos expr) reg table tables =
  cCheckTypeNum expr table tables 
  ++ (cExpr expr reg table tables)
  ++ indentation ++ "neg_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where 
    t = 
      case cGetExprBaseType expr table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> 
          error $ "RuntimeError: Cannot unary minus bool " ++ show pos
        StringType -> 
          error $ "RuntimeError: Cannot unary minus string " ++ show pos

-- check if num type
cCheckTypeNum :: Expr -> SymTable -> [SymTable] -> String
cCheckTypeNum expr table tables =
  case cGetExprBaseType expr table tables of
    IntType -> ""
    FloatType -> ""
    otherwise -> error $ "RuntimeError: Type error"
  
-- convert type
cConvertType :: Expr -> Expr -> Int -> SymTable -> [SymTable] -> String       
cConvertType expr1 expr2 reg table tables =
  case cGetExprBaseType expr1 table tables of
    IntType -> 
      case cGetExprBaseType expr2 table tables of
        FloatType -> 
          indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        IntType -> ""
        otherwise -> ""
    FloatType -> 
      case cGetExprBaseType expr2 table tables of
        FloatType -> ""
        IntType ->
          indentation ++ "int_to_real " 
          ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"
        otherwise -> ""
    otherwise -> ""

-- generate oz code of an expression
cExpr :: Expr -> Int -> SymTable -> [SymTable] -> String
cExpr (BoolCon pos const) reg table tables = cBoolConst const reg
cExpr (IntCon pos const) reg table tables = cIntConst const reg
cExpr (FloatCon pos const) reg table tables = cFloatConst const reg
cExpr (StrCon pos const) reg table tables = cStringConst const reg
cExpr (Id pos ident) reg table tables = 
  cId (Id pos ident) False reg table tables
cExpr (ArrayRef pos ident expr) reg table tables = 
  cArrayRef (ArrayRef pos ident expr) False reg table  tables
cExpr (MatrixRef pos ident expr1 expr2) reg table tables =
  cMatrixRef (MatrixRef pos ident expr1 expr2) False reg table tables
cExpr (And pos expr1 expr2) reg table tables = 
  cAnd (And pos expr1 expr2) reg table tables
cExpr (Or pos expr1 expr2) reg table tables = 
  cOr (Or pos expr1 expr2) reg table tables
cExpr (Not pos expr) reg table tables = 
  cNot (Not pos expr) reg table tables
cExpr (Rel pos relOp expr1 expr2) reg table tables = 
  cRel (Rel pos relOp expr1 expr2) reg table tables
cExpr (BinOpExp pos binOp expr1 expr2) reg table tables = 
  cBinOpExp (BinOpExp pos binOp expr1 expr2) reg table tables
cExpr (UnaryMinus pos expr) reg table tables = 
  cUnaryMinus (UnaryMinus pos expr) reg table tables

-- get base type of an expression
cGetExprBaseType :: Expr -> SymTable -> [SymTable] -> BaseType
cGetExprBaseType (BoolCon pos const) table tables = BoolType
cGetExprBaseType (IntCon pos const) table tables = IntType
cGetExprBaseType (FloatCon pos const) table tables = FloatType
cGetExprBaseType (StrCon pos const) table tables = StringType
cGetExprBaseType (Id pos ident) table tables = 
  cGetBaseType ident table tables
cGetExprBaseType (ArrayRef pos ident expr) table tables = 
  cGetBaseType ident table tables
cGetExprBaseType (MatrixRef pos ident expr1 expr2) table tables = 
  cGetBaseType ident table tables
cGetExprBaseType (And pos expr1 expr2) table tables = BoolType
cGetExprBaseType (Or pos expr1 expr2) table tables = BoolType
cGetExprBaseType (Not pos expr) table tables = BoolType
cGetExprBaseType (Rel pos relOp expr1 expr2) table tables = BoolType
cGetExprBaseType (BinOpExp pos binOp expr1 expr2) table tables = 
  case cGetExprBaseType expr1 table tables of
    IntType ->
      case cGetExprBaseType expr2 table tables of
        FloatType -> cGetExprBaseType expr2 table tables
        otherwise -> cGetExprBaseType expr1 table tables
    otherwise -> cGetExprBaseType expr1 table tables
cGetExprBaseType (UnaryMinus pos expr) table tables = 
  cGetExprBaseType expr table tables
