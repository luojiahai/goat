module CodeGenerator where

import GoatAST
import SymbolTable
import Control.Monad.State

indentation = "    "


-- generate oz code of a goat program
generate :: Program -> [SymTable] -> String
generate (Program procs) tables = 
  indentation ++ "call proc_main\n"
  ++ indentation ++ "halt"
  ++ procsStr
  where (_, procsStr) = gProcs procs 0 tables

-- generate oz code of a sequence of procedures
gProcs :: [Procedure] -> Int -> [SymTable] -> (Int, String)
gProcs [] label _ = (label, "")
gProcs [proc] label tables = 
  let (label', str) = gProc proc label tables 
  in (label', str)
gProcs (proc:procs) label tables = 
  let 
    (label', str) = gProc proc label tables 
    (label'', str') = gProcs procs label' tables
  in (label'', str ++ str')

-- generate oz code of a procedure
gProc :: Procedure -> Int -> [SymTable] -> (Int, String)
gProc (Procedure pos ident prmts decls stmts) label tables = 
  case stLookupSymTable ident tables of
    Just table ->
      (label', "\nproc_" ++ ident ++ ":\n"
      ++ gStackFrame "push" table tables
      ++ gPrmts prmts 0 table tables
      ++ gDecls decls table tables
      ++ stmtsStr
      ++ gStackFrame "pop" table tables
      ++ indentation ++ "return")
      where (label', stmtsStr) = gStmts stmts label table tables
    Nothing -> 
      error $ "RuntimeError: Procedure " 
      ++ ident ++ " not found " ++ show pos

-- generate oz code of push/pull stack frame
gStackFrame :: String -> SymTable -> [SymTable] -> String
gStackFrame command (SymTable header prmts hashMap) tables = 
  indentation ++ command ++ "_stack_frame" ++ " " 
  ++ show (stHashMapSize hashMap) ++ "\n"

-- generate oz code of a sequence of parameters
gPrmts :: [FormalArgSpec] -> Int -> SymTable -> [SymTable] -> String
gPrmts [] _ _ _ = ""
gPrmts [prmt] reg table tables = gPrmt prmt reg table tables
gPrmts (prmt:prmts) reg table tables = 
  gPrmt prmt reg table tables ++ gPrmts prmts (reg + 1) table tables

-- generate oz code of a parameter
gPrmt :: FormalArgSpec -> Int -> SymTable -> [SymTable] -> String
gPrmt (FormalArgSpec pos parMode baseType ident) reg table tables =
  case baseType of
    BoolType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = gGetSlot ident table tables
    IntType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = gGetSlot ident table tables
    FloatType -> 
      indentation ++ "store " 
      ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
      where slot = gGetSlot ident table tables

-- generate oz code of a sequence of declarations
gDecls :: [Decl] -> SymTable -> [SymTable] -> String
gDecls [] _ _ = ""
gDecls [decl] table tables = gDecl decl table tables
gDecls (decl:decls) table tables = 
  gDecl decl table tables ++ gDecls decls table tables

-- generate oz code of a declaration
gDecl :: Decl -> SymTable -> [SymTable] -> String
gDecl (Decl pos ident (Base baseType)) table tables =
  gInitBase (Decl pos ident (Base baseType)) table tables
gDecl (Decl pos ident (Array baseType i)) table tables = 
  gInitArray (Decl pos ident (Array baseType i)) table tables
gDecl (Decl pos ident (Matrix baseType i j)) table tables = 
  gInitMatrix (Decl pos ident (Matrix baseType i j)) table tables
    
-- generate oz code of initialization of base id
gInitBase :: Decl -> SymTable -> [SymTable] -> String
gInitBase (Decl pos ident (Base baseType)) table tables =
  case baseType of
    BoolType -> 
      gBoolConst False 0
      ++ gStore slot 0 1
      where slot = gGetSlot ident table tables
    IntType -> 
      gIntConst 0 0
      ++ gStore slot 0 1
      where slot = gGetSlot ident table tables
    FloatType -> 
      gFloatConst 0.0 0
      ++ gStore slot 0 1
      where slot = gGetSlot ident table tables

-- generate oz code of initialization of array id
gInitArray :: Decl -> SymTable -> [SymTable] -> String
gInitArray (Decl pos ident (Array baseType i)) table tables =
  case baseType of
    BoolType -> 
      gBoolConst False 0
      ++ gStore slot 0 i
      where slot = gGetSlot ident table tables
    IntType -> 
      gIntConst 0 0
      ++ gStore slot 0 i
      where slot = gGetSlot ident table tables
    FloatType -> 
      gFloatConst 0.0 0
      ++ gStore slot 0 i
      where slot = gGetSlot ident table tables

-- generate oz code of initialization of matrix id
gInitMatrix :: Decl -> SymTable -> [SymTable] -> String
gInitMatrix (Decl pos ident (Matrix baseType i j)) table tables =
  case baseType of
    BoolType -> 
      gBoolConst False 0
      ++ gStore slot 0 (i * j)
      where slot = gGetSlot ident table tables
    IntType -> 
      gIntConst 0 0
      ++ gStore slot 0 (i * j)
      where slot = gGetSlot ident table tables
    FloatType -> 
      gFloatConst 0.0 0
      ++ gStore slot 0 (i * j)
      where slot = gGetSlot ident table tables

-- generate oz code of store action
gStore :: Int -> Int -> Int -> String
gStore slot reg 0 = ""
gStore slot reg n = 
  indentation ++ "store " 
  ++ show slot ++ ", " ++ "r" ++ show reg ++ "\n"
  ++ gStore (slot + 1) reg (n - 1)

-- get slot number given an id
gGetSlot :: Ident -> SymTable -> [SymTable] -> Int
gGetSlot ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stASlot symbol of
        Just (ASlot slot) -> slot
        Nothing -> error $ "InternalError: gGetSlot"
    Nothing -> error $ "InternalError: gGetSlot " ++ ident

-- get base type given an id
gGetBaseType :: Ident -> SymTable -> [SymTable] -> BaseType
gGetBaseType ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stAType symbol of
        Just (AType baseType) -> baseType
        Nothing -> error $ "InternalError: gGetBaseType"
    Nothing -> error $ "InternalError: gGetBaseType " ++ ident

-- get goat type given an id
gGetGoatType :: Ident -> SymTable -> [SymTable] -> GoatType
gGetGoatType ident table tables =
  case stLookupHashMap ident table of
    Just symbol ->
      case stAGoatType symbol of
        Just (AGoatType goatType) -> goatType
        Nothing -> error $ "InternalError: gGetGoatType"
    Nothing -> error $ "InternalError: gGetGoatType " ++ ident

-- generate oz code of int constant
gIntConst :: Int -> Int -> String
gIntConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

-- generate oz code of float constant
gFloatConst :: Float -> Int -> String
gFloatConst const reg = 
  indentation ++ "real_const " ++ "r" ++ show reg
  ++ ", " ++ show const ++ "\n"

-- generate oz code of string constant
gStringConst :: String -> Int -> String
gStringConst const reg = 
  indentation ++ "string_const " ++ "r" ++ show reg
  ++ ", " ++ ('\"' : (const ++ "\"")) ++ "\n"

-- generate oz code of bool constant
gBoolConst :: Bool -> Int -> String
gBoolConst const reg = 
  indentation ++ "int_const " ++ "r" ++ show reg 
  ++ ", " ++
  case const of
    True -> show 1
    False -> show 0
  ++ "\n"

-- generate oz code of a sequence of statements
gStmts :: [Stmt] -> Int -> SymTable -> [SymTable] -> (Int, String)
gStmts [] label _ _ = (label, "")
gStmts [stmt] label table tables = gStmt stmt label table tables
gStmts (stmt:stmts) label table tables = 
  let 
    (label', str) = gStmt stmt label table tables 
    (label'', str') = gStmts stmts label' table tables
  in (label'', str ++ str')

-- generate oz code of a statement
gStmt :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
gStmt (Assign pos lvalue expr) label table tables = 
  (label, gAssign (Assign pos lvalue expr) table tables)
gStmt (Read pos lvalue) label table tables = 
  (label, gRead (Read pos lvalue) table tables)
gStmt (Write pos expr) label table tables = 
  (label, gWrite (Write pos expr) table tables)
gStmt (ProcCall pos ident exprs) label table tables = 
  (label, gProcCall (ProcCall pos ident exprs) table tables)
gStmt (If pos expr stmts) label table tables = 
  gIf (If pos expr stmts) label table tables
gStmt (IfElse pos expr stmts1 stmts2) label table tables = 
  gIfElse (IfElse pos expr stmts1 stmts2) label table tables
gStmt (While pos expr stmts) label table tables = 
  gWhile (While pos expr stmts) label table tables

-- generate oz code of assignment statement
gAssign :: Stmt -> SymTable -> [SymTable] -> String
gAssign (Assign pos lvalue expr) table tables =
  gExpr expr 0 table tables
  ++
  case gGetBaseType (gGetLvalueIdent lvalue) table tables of
    FloatType -> 
      case gGetExprBaseType expr table tables of
        IntType -> 
          indentation ++ "int_to_real " 
          ++ "r" ++ show 0 ++ ", " ++ "r" ++ show 0 ++ "\n"
        otherwise -> ""
    otherwise -> ""
  ++ gLvalue lvalue 0 table tables

-- generate oz code of read statement
gRead :: Stmt -> SymTable -> [SymTable] -> String
gRead (Read pos lvalue) table tables = 
  gCallBuiltin ("read_" ++ t) table tables
  ++ gLvalue lvalue 0 table tables
  where 
    t = 
      case gGetBaseType (gGetLvalueIdent lvalue) table tables of
        BoolType -> "bool"
        IntType -> "int"
        FloatType -> "real"
        StringType -> 
          error $ "RuntimeError: Cannot read string " ++ show pos

-- generate oz code of write statement
gWrite :: Stmt -> SymTable -> [SymTable] -> String
gWrite (Write pos expr) table tables = 
  gExpr expr 0 table tables
  ++ (gCallBuiltin ("print_" ++ t) table tables)
  where 
    t =
      case gGetExprBaseType expr table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> "bool"
        StringType -> "string"

-- generate oz code of procedure call statement
gProcCall :: Stmt -> SymTable -> [SymTable] -> String
gProcCall (ProcCall pos ident exprs) table tables = 
  gProcArgs ident exprs 0 0 table tables
  ++ indentation ++ "call proc_" ++ ident ++ "\n"

-- generate oz code of if statement
gIf :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
gIf (If pos expr stmts) label table tables = 
  (label', gExpr expr 0 table tables
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show label ++ "\n"
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ "label_" ++ show label ++ ":\n" ++ stmtsStr
  ++ "label_" ++ show (label + 1) ++ ":\n")
  where (label', stmtsStr) = gStmts stmts (label + 2) table tables

-- generate oz code of if-else statement
gIfElse :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
gIfElse (IfElse pos expr stmts1 stmts2) label table tables = 
  (label'', gExpr expr 0 table tables
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show label ++ "\n"
  ++ indentation ++ "branch_on_false " 
  ++ "r" ++ show 0 ++ ", " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ "label_" ++ show label ++ ":\n" ++ stmts1Str
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 2) ++ "\n"
  ++ "label_" ++ show (label + 1) ++ ":\n" ++ stmts2Str
  ++ "label_" ++ show (label + 2) ++ ":\n")
  where 
    (label', stmts1Str) = gStmts stmts1 (label + 3) table tables
    (label'', stmts2Str) = gStmts stmts2 label' table tables

-- generate oz code of while statement
gWhile :: Stmt -> Int -> SymTable -> [SymTable] -> (Int, String)
gWhile (While pos expr stmts) label table tables = 
  (label', "label_" ++ show label ++ ":\n"
  ++ (gExpr expr 1 table tables)
  ++ indentation ++ "branch_on_true "
  ++ "r" ++ show 1 ++ ", " ++ "label_" ++ show (label + 1) ++ "\n"
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show (label + 2) ++ "\n"
  ++ "label_" ++ show (label + 1) ++ ":\n" ++ stmtsStr
  ++ indentation ++ "branch_uncond " ++ "label_" ++ show label ++ "\n"
  ++ "label_" ++ show (label + 2)  ++ ":\n")
  where (label', stmtsStr) = gStmts stmts (label + 3) table tables

-- get identifier name of lvalue
gGetLvalueIdent :: Lvalue -> String
gGetLvalueIdent (LId pos ident) = ident
gGetLvalueIdent (LArrayRef pos ident expr) = ident
gGetLvalueIdent (LMatrixRef pos ident expr1 expr2) = ident

-- generate oz code of storing lvalue
gLvalue :: Lvalue -> Int -> SymTable -> [SymTable] -> String
gLvalue (LId pos ident) reg table tables = 
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
      error $ "InternalError: gLvalue " ++ ident ++ " " ++ show pos
  where slot = gGetSlot ident table tables
gLvalue (LArrayRef pos ident expr) reg table tables = 
  gExpr expr (reg + 2) table tables
  ++ indentation ++ "load_address " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ show slot ++ "\n"
  ++ indentation ++ "sub_offset " ++ "r" ++ show (reg + 1) ++ ", "
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 2) ++ "\n"
  ++ indentation ++ "store_indirect " 
  ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show reg ++ "\n"
  where slot = gGetSlot ident table tables
gLvalue (LMatrixRef pos ident expr1 expr2) reg table tables = 
  gExpr expr1 (reg + 2) table tables ++ gExpr expr2 (reg + 3) table tables
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
    slot = gGetSlot ident table tables
    len = 
      case gGetGoatType ident table tables of
        (Matrix baseType i j) -> j
        otherwise -> error $ "InternalError: gLvalue " ++ show pos

-- generate oz code of builtin function call
gCallBuiltin :: Ident -> SymTable -> [SymTable] -> String
gCallBuiltin ident table tables = 
  indentation ++ "call_builtin " ++ ident ++ "\n"

-- generate oz code of a sequence of procedure arguments
gProcArgs :: Ident -> [Expr] -> Int -> Int 
  -> SymTable -> [SymTable] -> String
gProcArgs _ [] _ _ _ _ = ""
gProcArgs ident [expr] i reg table tables = 
  gProcArg ident expr i reg table tables
gProcArgs ident (expr:exprs) i reg table tables = 
  gProcArg ident expr i reg table tables 
  ++ gProcArgs ident exprs (i + 1) (reg + 1) table tables

-- generate oz code of a procedure argument
gProcArg :: Ident -> Expr -> Int -> Int 
  -> SymTable -> [SymTable] -> String
gProcArg ident expr i reg table tables = 
  case stAParMode symbol of
    Just (AParMode parMode) ->
      case parMode of
        Val -> 
          case stAType symbol of
            Just (AType baseType) -> 
              gProcArgVal ident baseType expr reg table tables
            Nothing -> error $ "InternalError: gProcArg"
        Ref -> 
          case stAType symbol of
            Just (AType baseType) -> 
              gProcArgRef ident baseType expr reg table tables
            Nothing -> error $ "InternalError: gProcArg"
    Nothing -> error $ "InternalError: gProcArg"
    where 
      symbol = 
        case stLookupSymTable ident tables of
          Just table' -> stGetArgSymbol i table'
          Nothing -> error $ "InternalError: gProcArg"
  
-- generate oz code of a procedure argument with parmode val
gProcArgVal :: Ident -> BaseType -> Expr -> Int 
  -> SymTable -> [SymTable] -> String
gProcArgVal ident baseType expr reg table tables =
  if (gGetExprBaseType expr table tables) == baseType 
  then gExpr expr reg table tables
  else 
    case baseType of
      FloatType ->
        if (gGetExprBaseType expr table tables) == IntType
        then 
          gExpr expr reg table tables
          ++ indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        else
          error $ "RuntimeError: Type error"
      otherwise -> error $ "RuntimeError: Type error"

-- generate oz code of a procedure argument with parmode ref
gProcArgRef :: Ident -> BaseType -> Expr -> Int 
  -> SymTable -> [SymTable] -> String
gProcArgRef ident baseType expr reg table tables =
  if (gGetExprBaseType expr table tables) == baseType 
  then gExprAddr expr reg table tables
  else
    case baseType of
      FloatType ->
        if (gGetExprBaseType expr table tables) == IntType
        then 
          gExprAddr expr reg table tables
          ++ indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        else
          error $ "RuntimeError: Type error"
      otherwise -> error $ "RuntimeError: Type error"

-- generate oz code of an expression as ref
gExprAddr :: Expr -> Int -> SymTable -> [SymTable] -> String
gExprAddr (Id pos ident) reg table tables = 
  gId (Id pos ident) True reg table tables
gExprAddr (ArrayRef pos ident expr) reg table tables = 
  gArrayRef (ArrayRef pos ident expr) True reg table  tables
gExprAddr (MatrixRef pos ident expr1 expr2) reg table tables =
  gMatrixRef (MatrixRef pos ident expr1 expr2) True reg table tables
gExprAddr _ _ _ _ = 
  error $ "RuntimeError: Ref must be Id/ArrayRef/MatrixRef"

-- generate oz code of loading id
gId :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
gId (Id pos ident) isLoadAddr reg table tables =
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
      error $ "InternalError: gId " ++ ident ++ " " ++ show pos
  where slot = gGetSlot ident table tables

-- generate oz code of loading array reference
gArrayRef :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
gArrayRef (ArrayRef pos ident expr) isLoadAddr reg table tables =
  gExpr expr (reg + 1) table tables
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
  where slot = gGetSlot ident table tables

-- generate oz code of loading matrix reference
gMatrixRef :: Expr -> Bool -> Int -> SymTable -> [SymTable] -> String
gMatrixRef (MatrixRef pos ident expr1 expr2) isLoadAddr reg table tables = 
  gExpr expr1 (reg + 1) table tables ++ gExpr expr2 (reg + 2) table tables
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
    slot = gGetSlot ident table tables
    len = 
      case gGetGoatType ident table tables of
        (Matrix baseType i j) -> j
        otherwise -> error $ "InternalError: gMatrixRef " ++ show pos

-- generate oz code of and expression
gAnd :: Expr -> Int -> SymTable -> [SymTable] -> String
gAnd (And pos expr1 expr2) reg table tables =
  gExpr expr1 reg table tables
  ++ (gExpr expr2 (reg + 1) table tables)
  ++ indentation ++ "and " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

-- generate oz code of or expression
gOr :: Expr -> Int -> SymTable -> [SymTable] -> String
gOr (Or pos expr1 expr2) reg table tables =
  gExpr expr1 reg table tables
  ++ (gExpr expr2 (reg + 1) table tables)
  ++ indentation ++ "or " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"

-- generate oz code of not expression
gNot :: Expr -> Int -> SymTable -> [SymTable] -> String
gNot (Not pos expr) reg table tables =
  gExpr expr reg table tables
  ++ indentation ++ "not " ++ "r" ++ show reg ++ ", " 
  ++ "r" ++ show reg ++ "\n"

-- generate oz code of rel expression
gRel :: Expr -> Int -> SymTable -> [SymTable] -> String
gRel (Rel pos relOp expr1 expr2) reg table tables =
  gExpr expr1 reg table tables
  ++ (gExpr expr2 (reg + 1) table tables)
  ++ (gConvertType expr1 expr2 reg table tables)
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
      case gGetExprBaseType expr1 table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> "int"
        StringType -> 
          error $ "RuntimeError: Cannot compare string " ++ show pos

-- generate oz code of binary operation expression
gBinOpExp :: Expr -> Int -> SymTable -> [SymTable] -> String
gBinOpExp (BinOpExp pos binOp expr1 expr2) reg table tables =
  gCheckTypeNum expr1 table tables 
  ++ (gExpr expr1 reg table tables) ++ (gExpr expr2 (reg + 1) table tables)
  ++ (gConvertType expr1 expr2 reg table tables)
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
      case gGetExprBaseType expr1 table tables of
        IntType -> 
          case gGetExprBaseType expr2 table tables of
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
gUnaryMinus :: Expr -> Int -> SymTable -> [SymTable] -> String
gUnaryMinus (UnaryMinus pos expr) reg table tables =
  gCheckTypeNum expr table tables 
  ++ (gExpr expr reg table tables)
  ++ indentation ++ "neg_" ++ t ++ " "
  ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
  where 
    t = 
      case gGetExprBaseType expr table tables of
        IntType -> "int"
        FloatType -> "real"
        BoolType -> 
          error $ "RuntimeError: Cannot unary minus bool " ++ show pos
        StringType -> 
          error $ "RuntimeError: Cannot unary minus string " ++ show pos

-- check if num type
gCheckTypeNum :: Expr -> SymTable -> [SymTable] -> String
gCheckTypeNum expr table tables =
  case gGetExprBaseType expr table tables of
    IntType -> ""
    FloatType -> ""
    otherwise -> error $ "RuntimeError: Type error"
  
-- convert type
gConvertType :: Expr -> Expr -> Int -> SymTable -> [SymTable] -> String       
gConvertType expr1 expr2 reg table tables =
  case gGetExprBaseType expr1 table tables of
    IntType -> 
      case gGetExprBaseType expr2 table tables of
        FloatType -> 
          indentation ++ "int_to_real " 
          ++ "r" ++ show reg ++ ", " ++ "r" ++ show reg ++ "\n"
        IntType -> ""
        otherwise -> ""
    FloatType -> 
      case gGetExprBaseType expr2 table tables of
        FloatType -> ""
        IntType ->
          indentation ++ "int_to_real " 
          ++ "r" ++ show (reg + 1) ++ ", " ++ "r" ++ show (reg + 1) ++ "\n"
        otherwise -> ""
    otherwise -> ""

-- generate oz code of an expression
gExpr :: Expr -> Int -> SymTable -> [SymTable] -> String
gExpr (BoolCon pos const) reg table tables = gBoolConst const reg
gExpr (IntCon pos const) reg table tables = gIntConst const reg
gExpr (FloatCon pos const) reg table tables = gFloatConst const reg
gExpr (StrCon pos const) reg table tables = gStringConst const reg
gExpr (Id pos ident) reg table tables = 
  gId (Id pos ident) False reg table tables
gExpr (ArrayRef pos ident expr) reg table tables = 
  gArrayRef (ArrayRef pos ident expr) False reg table  tables
gExpr (MatrixRef pos ident expr1 expr2) reg table tables =
  gMatrixRef (MatrixRef pos ident expr1 expr2) False reg table tables
gExpr (And pos expr1 expr2) reg table tables = 
  gAnd (And pos expr1 expr2) reg table tables
gExpr (Or pos expr1 expr2) reg table tables = 
  gOr (Or pos expr1 expr2) reg table tables
gExpr (Not pos expr) reg table tables = 
  gNot (Not pos expr) reg table tables
gExpr (Rel pos relOp expr1 expr2) reg table tables = 
  gRel (Rel pos relOp expr1 expr2) reg table tables
gExpr (BinOpExp pos binOp expr1 expr2) reg table tables = 
  gBinOpExp (BinOpExp pos binOp expr1 expr2) reg table tables
gExpr (UnaryMinus pos expr) reg table tables = 
  gUnaryMinus (UnaryMinus pos expr) reg table tables

-- get base type of an expression
gGetExprBaseType :: Expr -> SymTable -> [SymTable] -> BaseType
gGetExprBaseType (BoolCon pos const) table tables = BoolType
gGetExprBaseType (IntCon pos const) table tables = IntType
gGetExprBaseType (FloatCon pos const) table tables = FloatType
gGetExprBaseType (StrCon pos const) table tables = StringType
gGetExprBaseType (Id pos ident) table tables = 
  gGetBaseType ident table tables
gGetExprBaseType (ArrayRef pos ident expr) table tables = 
  gGetBaseType ident table tables
gGetExprBaseType (MatrixRef pos ident expr1 expr2) table tables = 
  gGetBaseType ident table tables
gGetExprBaseType (And pos expr1 expr2) table tables = BoolType
gGetExprBaseType (Or pos expr1 expr2) table tables = BoolType
gGetExprBaseType (Not pos expr) table tables = BoolType
gGetExprBaseType (Rel pos relOp expr1 expr2) table tables = BoolType
gGetExprBaseType (BinOpExp pos binOp expr1 expr2) table tables = 
  case gGetExprBaseType expr1 table tables of
    IntType ->
      case gGetExprBaseType expr2 table tables of
        FloatType -> gGetExprBaseType expr2 table tables
        otherwise -> gGetExprBaseType expr1 table tables
    otherwise -> gGetExprBaseType expr1 table tables
gGetExprBaseType (UnaryMinus pos expr) table tables = 
  gGetExprBaseType expr table tables
