module GoatPrettyPrinter where

import GoatAST


-----------------------------------------------------------------
-- pretty printer
-- main printer
-----------------------------------------------------------------
indentation :: String
indentation = "    "

 -- Two procedure should be sep by a blank line
prettyPrint :: GoatProgram -> IO()
prettyPrint (GoatProgram procs)
  = pprintBySepNoIndent printProc procs "\n"

-----------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------

-- A printer that behaves similar to foldr, except not using accumulator.
-- Basically will call printer then print sep, except on last element
-- for which it will not print sep sign
pprintBySep :: String -> (String -> a -> IO()) -> [a] -> String  -> IO()
pprintBySep _ _ [] _  = return ()
pprintBySep indent printer [x] _ = printer indent x 
pprintBySep indent printer (x:xs) sep = 
  do 
    printer indent x 
    putStr sep
    pprintBySep indent printer xs sep

-- Some printer function does not take indent as input, but still wants
-- to user setPrinter function. Thus we wrap it arround to take an additional
-- argument
pprintBySepNoIndent :: (a -> IO()) -> [a] -> String  -> IO()
pprintBySepNoIndent printer xs sep = pprintBySep "" (\_ -> printer) xs sep

-----------------------------------------------------------------
-- all other subprinters
-----------------------------------------------------------------

-- procedure printers
printProc :: Procedure -> IO()
printProc (Main decl stmt) = printProc (Procedure "main" [] decl stmt)
printProc (Procedure id prmts decls stmts) = 
  do
    putStr "proc "
    putStr $ id ++ " ("
    pprintBySepNoIndent pprintPrmt prmts ", "
    putStrLn ")"
    pprintBySep indentation pprintDecl decls ""
    putStrLn "begin"
    pprintBySep indentation pprintStmt stmts ""
    putStrLn "end"

-- A printer that can handle priting a single prmteter in proc 
pprintPrmt :: Prmt -> IO()
pprintPrmt (Prmt prmtIndicator basetype id) = 
  do
    case prmtIndicator of
      Val -> putStr "val "
      Ref -> putStr "ref "
    pprintBaseType basetype
    putStr id

-- Printer for basetypes
pprintBaseType :: BaseType -> IO()
pprintBaseType t =
  case t of
    BoolType -> putStr "bool "
    IntType -> putStr "int "
    FloatType -> putStr "float "
    
-- Declaration printers
pprintDecl :: String -> Decl -> IO()
pprintDecl indent (Decl idname basetype) = 
  do
    putStr indent
    pprintBaseType basetype
    pprintIdName idname
    putStrLn ";"

-- print IdName and IdNameWithShape
pprintIdName :: IdName -> IO()
pprintIdName (Name id) = putStr id
pprintIdName (NameWithShape id exprs) = 
  do
    putStr $ id ++ "["
    pprintBySepNoIndent pprintExpr exprs ", "
    putStr "]"
    
-- print a single recursive statment with indentation accumulator
pprintStmt :: String -> Stmt -> IO()
pprintStmt indentAcc stmt = 
  do
    putStr indentAcc
    pprintStmt' indentAcc stmt

-- Function that will print statment itself
-- Just so that I dont have to write putStr indentAcc
-- In every single function...
pprintStmt' :: String -> Stmt -> IO()
pprintStmt' _ (Assign (LId idName) expr) = 
  do
    pprintIdName idName
    putStr " := "
    pprintExpr expr
    putStrLn ";"
      
pprintStmt' _ (Read (LId idName)) =
  do
    putStr ("read ")
    pprintIdName idName
    putStrLn (";")

pprintStmt' _ (Write expr) =
  do
    putStr ("write ")
    pprintExpr expr
    putStrLn (";")

pprintStmt' _ (Call id exprs) = 
  do
    putStr ("call " ++ id ++ "(")
    pprintBySepNoIndent pprintExpr exprs ", "
    putStrLn (");")
  
pprintStmt' indentAcc (If expr stmts) = 
  do
    putStr ("if ")
    pprintExpr expr
    putStrLn (" then")
    pprintBySep (indentAcc ++ indentation) pprintStmt stmts ""
    putStrLn (indentAcc ++ "fi")

pprintStmt' indentAcc (IfElse expr stmts1 stmts2) = 
  do
    putStr ("if ")
    pprintExpr expr
    putStrLn (" then")
    pprintBySep (indentAcc ++ indentation) pprintStmt stmts1 ""
    putStrLn (indentAcc ++ "else")
    pprintBySep (indentAcc ++ indentation) pprintStmt stmts2 ""
    putStrLn (indentAcc ++ "fi")

pprintStmt' indentAcc (While expr stmts) = 
  do
    putStr ("while ")
    pprintExpr expr
    putStrLn (" do")
    pprintBySep (indentAcc ++ indentation) pprintStmt stmts ""
    putStrLn (indentAcc ++ "od")

-- Expression printers. One for each type
pprintExpr :: Expr -> IO()
pprintExpr (IntConst i)      = putStr (show i)
pprintExpr (StrConst s)      = putStr (show s)
pprintExpr (FloatConst f)    = putStr (show f)
pprintExpr (Id idname)       = pprintIdName idname
pprintExpr (BoolConst bool)  = putStr (show bool)
pprintExpr (UnExpr unop expr) = pprintUnExpr unop expr
pprintExpr (BinExpr binop expr1 expr2) = pprintBinExpr binop expr1 expr2

pprintUnExpr :: UnOp -> Expr -> IO ()
pprintUnExpr unop expr =
  do
    pprintUnOp unop
    pprintExpr expr

pprintBinExpr :: BinOp -> Expr -> Expr -> IO ()
pprintBinExpr binop expr1 expr2 =
  do
    pprintExprParens expr1
    pprintBinOp binop
    pprintExprParens expr2

-- Will check if the expr needs to be surrend by parens
pprintExprParens :: Expr -> IO()
pprintExprParens expr = 
  case expr of
    BinExpr _ _ _ -> do
                        putStr "("
                        pprintExpr expr
                        putStr ")"
    otherwise -> pprintExpr expr

pprintUnOp :: UnOp -> IO ()
pprintUnOp unop =
  case unop of
    Negative -> putStr "-"
    Not      -> putStr "!"

pprintBinOp :: BinOp -> IO()
pprintBinOp binop = 
  case binop of
    Equal        -> putStr " = "
    Greater      -> putStr " > "
    GreaterEqual -> putStr " >= "
    Less         -> putStr " < "
    LessEqual    -> putStr " <= "
    NotEqual     -> putStr " != "
    And          -> putStr " && "
    Or           -> putStr " || "
    Add          -> putStr " + "
    Multiply     -> putStr " * "
    Subtract     -> putStr " - "
    Divide       -> putStr " / "
