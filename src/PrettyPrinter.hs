module PrettyPrinter where

import GoatAST
-----------------------------------------------------------------
-- pretty printer
-- Most printer will take indent as an input
-----------------------------------------------------------------

indentation :: String
indentation = "    "

 -- Two procedure should be sep by a blank line
prettyPrinter :: GoatProgram -> IO()
prettyPrinter (GoatProgram procs)
  = sepPrinterNoIdent procedurePrinter procs "\n"

procedurePrinter :: Procedure -> IO()
procedurePrinter (Main decl stmt) = procedurePrinter (Procedure "main" [] decl stmt)
procedurePrinter (Procedure id params decls stmts)
  = do
      putStr "proc "
      putStr $ id ++ " ("
      sepPrinter "" paramPrinter params ", "
      putStrLn ")"
      sepPrinter indentation declPrinter decls ""
      putStrLn "begin"
      sepPrinter indentation stmtPrinter stmts ""
      putStrLn "end"

-- stmt printer with indentation accumulator
stmtPrinter :: String -> Stmt -> IO()
stmtPrinter indentAcc stmt
  = do
      putStr indentAcc
      stmtPrinter' indentAcc stmt

-- Function that will print statment itself
-- Just so that I dont have to write putStr indentAcc
-- In every single function...
stmtPrinter' :: String -> Stmt -> IO()
stmtPrinter' _ (Assign (LId idName) expr)
  = do
      idNamePrinter idName
      putStr " := "
      exprPrinter expr
      putStrLn ";"
      
stmtPrinter' _ (Read (LId idName))
  = do
    putStr ("read ")
    idNamePrinter idName
    putStrLn (";")

stmtPrinter' _ (Write expr)
  = do
    putStr ("write ")
    exprPrinter expr
    putStrLn (";")


stmtPrinter' _ (Call id exprs)
  = do
    putStr ("call " ++ id ++ "(")
    sepPrinterNoIdent exprPrinter exprs ", "
    putStrLn (");")
  
  
stmtPrinter' indentAcc (If expr stmts)
  = do
    putStr ("if ")
    exprPrinter expr
    putStrLn (" then")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts ""
    putStrLn (indentAcc ++ "fi")

stmtPrinter' indentAcc (IfElse expr stmts1 stmts2)
  = do
    putStr ("if ")
    exprPrinter expr
    putStrLn (" then")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts1 ""
    putStrLn (indentAcc ++ "else")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts2 ""
    putStrLn (indentAcc ++ "fi")

stmtPrinter' indentAcc (While expr stmts)
  = do
    putStr ("while ")
    exprPrinter expr
    putStrLn (" do")
    sepPrinter (indentAcc ++ indentation) stmtPrinter stmts ""
    putStrLn (indentAcc ++ "od")
-- Expression printer 
-- Although it does take indentation as an input
-- The printer will just simply ignore it, since in no case
-- this will be usefull. Indentation argument here is purely for the
-- purpose that it can be called by 
exprPrinter :: Expr -> IO()
exprPrinter (IntConst i) = putStr (show i)
exprPrinter (StrConst s) = putStr s
exprPrinter (Num f) = putStr (show f)
exprPrinter (Id idname) = idNamePrinter idname
exprPrinter (BoolConst bool) = putStr (show bool)
exprPrinter (UnaryNot expr) = (putStr "!" >> exprPrinter expr)
exprPrinter (UnaryMinus expr) = (putStr "-" >> exprPrinter expr)
exprPrinter (BinExpr binop expr1 expr2) 
  = do
      exprParenPrinter expr1
      binOpPrinter binop
      exprParenPrinter expr2

-- Will check if the expr needs to be surrend by parens
exprParenPrinter :: Expr -> IO()
exprParenPrinter expr
  = case expr of
      BinExpr _ _ _ -> do
                          putStr("(")
                          exprPrinter expr
                          putStr(")")
      _ -> exprPrinter expr

binOpPrinter :: BinOp -> IO()
binOpPrinter binop
  = case binop of
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

paramPrinter :: String -> Param -> IO()
paramPrinter _ (Param paramIndicator basetype id)
  = do
      case paramIndicator of
        Val -> putStr "val "
        Ref -> putStr "ref "
      baseTypePrinter basetype
      putStr id

baseTypePrinter :: BaseType -> IO()
baseTypePrinter t 
  = case t of
      BoolType -> putStr "bool "
      IntType -> putStr "int "
      FloatType -> putStr "float "

declPrinter :: String -> Decl -> IO()
declPrinter indent (Decl idname basetype)
  = do
      putStr indent
      baseTypePrinter basetype
      idNamePrinter idname
      putStrLn ";"

idNamePrinter :: IdName -> IO()
idNamePrinter (Name id) = putStr id
idNamePrinter (NameWithShape id exprs)
  = do
      putStr $ id ++ "["
      sepPrinterNoIdent exprPrinter exprs ", "
      putStr "]"

-- use supplied print function to print list of item sperated with sep sign
-- start with indentation
-- i.e. sepPrinter "" param paramPrinter ", " will print parameters sep by ", "
sepPrinter :: String -> (String -> a -> IO()) -> [a] -> String  -> IO()
sepPrinter _ _ [] _  = return ()
sepPrinter indent printer [x] _ = printer indent x 
sepPrinter indent printer (x:xs) sep
  = do 
      printer indent x 
      putStr sep
      sepPrinter indent printer xs sep

-- sepPrinter without indent
sepPrinterNoIdent :: (a -> IO()) -> [a] -> String  -> IO()
sepPrinterNoIdent printer xs sep = sepPrinter "" (\_ -> printer) xs sep
