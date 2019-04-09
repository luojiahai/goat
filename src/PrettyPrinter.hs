module PrettyPrinter where

import GoatAST



-----------------------------------------------------------------
-- pretty printer
-- main printer
-----------------------------------------------------------------
indentation :: String
indentation = "    "

 -- Two procedure should be sep by a blank line
prettyPrinter :: GoatProgram -> IO()
prettyPrinter (GoatProgram procs)
  = sepPrinterNoIdent procedurePrinter procs "\n"

-----------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------

-- A printer that behaves similar to foldr, except not using accumulator.
-- Basically will call printer then print sep, except on last element
-- for which it will not print sep sign
sepPrinter :: String -> (String -> a -> IO()) -> [a] -> String  -> IO()
sepPrinter _ _ [] _  = return ()
sepPrinter indent printer [x] _ = printer indent x 
sepPrinter indent printer (x:xs) sep = 
  do 
    printer indent x 
    putStr sep
    sepPrinter indent printer xs sep

-- Some printer function does not take indent as input, but still wants
-- to user setPrinter function. Thus we wrap it arround to take an additional
-- argument
sepPrinterNoIdent :: (a -> IO()) -> [a] -> String  -> IO()
sepPrinterNoIdent printer xs sep = sepPrinter "" (\_ -> printer) xs sep


-----------------------------------------------------------------
-- all other subprinters
-----------------------------------------------------------------

-- procedure printers
procedurePrinter :: Procedure -> IO()
procedurePrinter (Main decl stmt) = procedurePrinter (Procedure "main" [] decl stmt)
procedurePrinter (Procedure id params decls stmts) = 
  do
    putStr "proc "
    putStr $ id ++ " ("
    sepPrinterNoIdent paramPrinter params ", "
    putStrLn ")"
    sepPrinter indentation declPrinter decls ""
    putStrLn "begin"
    sepPrinter indentation stmtPrinter stmts ""
    putStrLn "end"

-- A printer that can handle priting a single parameter in proc 
paramPrinter :: String -> Param -> IO()
paramPrinter (Param paramIndicator basetype id) = 
  do
    case paramIndicator of
      Val -> putStr "val "
      Ref -> putStr "ref "
    baseTypePrinter basetype
    putStr id

-- Printer for basetypes
baseTypePrinter :: BaseType -> IO()
baseTypePrinter t =
  case t of
    BoolType -> putStr "bool "
    IntType -> putStr "int "
    FloatType -> putStr "float "
    
-- Declaration printers
declPrinter :: String -> Decl -> IO()
declPrinter indent (Decl idname basetype) = 
  do
    putStr indent
    baseTypePrinter basetype
    idNamePrinter idname
    putStrLn ";"

-- print IdName and IdNameWithShape
idNamePrinter :: IdName -> IO()
idNamePrinter (Name id) = putStr id
idNamePrinter (NameWithShape id exprs) = 
  do
    putStr $ id ++ "["
    sepPrinterNoIdent exprPrinter exprs ", "
    putStr "]"
    
-- print a single recursive statment with indentation accumulator
stmtPrinter :: String -> Stmt -> IO()
stmtPrinter indentAcc stmt = 
  do
    putStr indentAcc
    stmtPrinter' indentAcc stmt

-- Function that will print statment itself
-- Just so that I dont have to write putStr indentAcc
-- In every single function...
stmtPrinter' :: String -> Stmt -> IO()
stmtPrinter' _ (Assign (LId idName) expr) = 
  do
    idNamePrinter idName
    putStr " := "
    exprPrinter expr
    putStrLn ";"
      
stmtPrinter' _ (Read (LId idName)) =
  do
    putStr ("read ")
    idNamePrinter idName
    putStrLn (";")

stmtPrinter' _ (Write expr) =
  do
    putStr ("write ")
    exprPrinter expr
    putStrLn (";")

stmtPrinter' _ (Call id exprs) = 
  do
    putStr ("call " ++ id ++ "(")
    sepPrinterNoIdent exprPrinter exprs ", "
    putStrLn (");")
  
stmtPrinter' indentAcc (If expr stmts) = 
  do
    putStr ("if ")
    exprPrinter expr
    putStrLn (" then")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts ""
    putStrLn (indentAcc ++ "fi")

stmtPrinter' indentAcc (IfElse expr stmts1 stmts2) = 
  do
    putStr ("if ")
    exprPrinter expr
    putStrLn (" then")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts1 ""
    putStrLn (indentAcc ++ "else")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts2 ""
    putStrLn (indentAcc ++ "fi")

stmtPrinter' indentAcc (While expr stmts) = 
  do
    putStr ("while ")
    exprPrinter expr
    putStrLn (" do")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts ""
    putStrLn (indentAcc ++ "od")

-- Expression printers. One for each type
exprPrinter :: Expr -> IO()
exprPrinter (IntConst i) = putStr (show i)
exprPrinter (StrConst s) = putStr s
exprPrinter (Num f) = putStr (show f)
exprPrinter (Id idname) = idNamePrinter idname
exprPrinter (BoolConst bool) = putStr (show bool)
exprPrinter (UnaryNot expr) = (putStr "!" >> exprPrinter expr)
exprPrinter (UnaryMinus expr) = (putStr "-" >> exprPrinter expr)
exprPrinter (BinExpr binop expr1 expr2) = 
  do
    exprParenPrinter expr1
    binOpPrinter binop
    exprParenPrinter expr2

-- Will check if the expr needs to be surrend by parens
exprParenPrinter :: Expr -> IO()
exprParenPrinter expr = 
  case expr of
    BinExpr _ _ _ -> do
                        putStr("(")
                        exprPrinter expr
                        putStr(")")
    otherwise -> exprPrinter expr

binOpPrinter :: BinOp -> IO()
binOpPrinter binop = 
  case binop of
    Equ -> putStr " = "
    Greater -> putStr " > "
    GreaterEqu -> putStr " >= "
    Less -> putStr " < "
    LessEqu -> putStr " <= "
    NotEqu -> putStr " != "
    And -> putStr " && "
    Or -> putStr " || "
    Add -> putStr " + "
    Mul -> putStr " * "
    Sub -> putStr " - "
    Div -> putStr " / "

