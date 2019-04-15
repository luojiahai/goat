module GoatPrettyPrinter where

import GoatAST


-- Pre-defines number of spaces for an indentation
indentation :: String
indentation = "    "

-- Pretty-Prints a goat program
--   Two procedure should be sep by a blank line
prettyPrint :: GoatProgram -> IO ()
prettyPrint (GoatProgram procs)
  = pprintBySepNoIndent printProc procs "\n"

-- Processes a print action seperated by a seperator
--   This is a helper function that behaves similar to foldr, 
--   except not using accumulator.
--   Basically this will call a print action then print sep, except on 
--   last element for which it will not print sep sign.
pprintBySep :: String -> (String -> a -> IO ()) -> [a] -> String  -> IO ()
pprintBySep _ _ [] _  = return ()
pprintBySep indent printAction [x] _ = printAction indent x 
pprintBySep indent printAction (x:xs) sep = 
  do 
    printAction indent x 
    putStr sep
    pprintBySep indent printAction xs sep

-- Processes a print action seperated by a seperator without indentations
--   Some print actions does not take indent as input, but still wants
--   to use the sep function. Thus we wrap it arround to take 
--   an additional argument.
pprintBySepNoIndent :: (a -> IO ()) -> [a] -> String  -> IO ()
pprintBySepNoIndent printAction xs sep = pprintBySep "" (\_ -> printAction) xs sep

-- Prints a procedure
printProc :: Procedure -> IO ()
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

-- Prints a parameter
--   This is a print action that can handle priting a single prmteter in a procedure.
pprintPrmt :: Prmt -> IO ()
pprintPrmt (Prmt prmtIndicator basetype id) = 
  do
    case prmtIndicator of
      Val -> putStr "val "
      Ref -> putStr "ref "
    pprintBaseType basetype
    putStr id

-- Prints a base type
pprintBaseType :: BaseType -> IO ()
pprintBaseType t =
  case t of
    BoolType  -> putStr "bool "
    IntType   -> putStr "int "
    FloatType -> putStr "float "
    
-- Prints a declaration
pprintDecl :: String -> Decl -> IO ()
pprintDecl indent (Decl ident basetype) = 
  do
    putStr indent
    pprintBaseType basetype
    pprintIdent ident
    putStrLn ";"

-- Prints an identifier
pprintIdent :: Ident -> IO ()
pprintIdent (Ident name) = putStr name
pprintIdent (IdentWithShape name exprs) = 
  do
    putStr $ name ++ "["
    pprintBySepNoIndent pprintExpr exprs ", "
    putStr "]"
    
-- Prints a statement
--   This action processes a single recursive statment 
--   with indentation accumulator.
pprintStmt :: String -> Stmt -> IO ()
pprintStmt indentAcc stmt = 
  do
    putStr indentAcc
    pprintStmt' indentAcc stmt

-- Prints a/an assginment/read/write/call/ifthen/ifthenelse/while statement
--   This function will print statment itself 
--   with indentation accumulator.
pprintStmt' :: String -> Stmt -> IO ()
pprintStmt' _ (Assign (LId idName) expr) = 
  do
    pprintIdent idName
    putStr " := "
    pprintExpr expr
    putStrLn ";"
      
pprintStmt' _ (Read (LId idName)) =
  do
    putStr ("read ")
    pprintIdent idName
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
  
pprintStmt' indentAcc (IfThen expr stmts) = 
  do
    putStr ("if ")
    pprintExpr expr
    putStrLn (" then")
    pprintBySep (indentAcc ++ indentation) pprintStmt stmts ""
    putStrLn (indentAcc ++ "fi")

pprintStmt' indentAcc (IfThenElse expr stmts1 stmts2) = 
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

-- Prints an expression
pprintExpr :: Expr -> IO ()
pprintExpr (IntConst i)      = putStr (show i)
pprintExpr (StrConst s)      = putStr (show s)
pprintExpr (FloatConst f)    = putStr (show f)
pprintExpr (Id ident)       = pprintIdent ident
pprintExpr (BoolConst bool)  = putStr (show bool)
pprintExpr (UnExpr unop expr) = pprintUnExpr unop expr
pprintExpr (BinExpr binop expr1 expr2) = pprintBinExpr binop expr1 expr2

-- Prints a unary experssion
pprintUnExpr :: UnOp -> Expr -> IO ()
pprintUnExpr unop expr =
  do
    pprintUnOp unop
    pprintExpr expr

-- Prints a binary expression
pprintBinExpr :: BinOp -> Expr -> Expr -> IO ()
pprintBinExpr binop expr1 expr2 =
  do
    pprintExprParens expr1
    pprintBinOp binop
    pprintExprParens expr2

-- Prints an expression with parentheses
--   This function will check if the expr needs to be surrend by parentheses.
pprintExprParens :: Expr -> IO ()
pprintExprParens expr = 
  case expr of
    BinExpr _ _ _ -> do
                        putStr "("
                        pprintExpr expr
                        putStr ")"
    otherwise -> pprintExpr expr

-- Prints a unary operator
pprintUnOp :: UnOp -> IO ()
pprintUnOp unop =
  case unop of
    Negative -> putStr "-"
    Not      -> putStr "!"

-- Prints a binary operator
pprintBinOp :: BinOp -> IO ()
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