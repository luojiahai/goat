module Main where 

import GoatAST
import Data.Char
import Debug.Trace
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
   = Parsec String Int a

-- Parser part. Initial Declaration

lexer :: Q.TokenParser Int
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine       = "#"
    , Q.nestedComments    = True
    , Q.identStart        = letter
    , Q.identLetter       = alphaNum
    , Q.opStart           = oneOf "+-*:/|&!<>"
    , Q.opLetter          = oneOf "+-*:/|&!<>"
    , Q.reservedNames     = [ "begin"
                            , "bool"
                            , "call"
                            , "do"
                            , "else"
                            , "end"
                            , "false"
                            , "fi"
                            , "float"
                            , "if"
                            , "int"
                            , "od"
                            , "proc"
                            , "read"
                            , "ref"
                            , "then"
                            , "true"
                            , "val"
                            , "while"
                            , "write"
                            ]
    , Q.reservedOpNames     = [ "||"
                            , "&&"
                            , "!"
                            , "="
                            , ":="
                            , "!="
                            , "<"
                            , "<="
                            , ">"
                            , ">="
                            , "+"
                            , "-"
                            , "*"
                            , "/"
                            ]
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
brackets   = Q.brackets lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer
symbol     = Q.symbol lexer





-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
  = do
      procs <- many1 pProc
      return $ GoatProgram procs
      

pProc :: Parser Procedure
pProc
  = do
      reserved "proc"
      do
        (do
          reserved "main"
          parens (return ())
          (decls,stmts) <- pProgBody
          return $ Main decls stmts)
        <|>
        (do
          ident <- identifier
          params <- parens (pParam `sepBy` (symbol ","))
          (decls,stmts) <- pProgBody
          return $ Procedure ident params decls stmts)

pParam :: Parser Param
pParam
  = do
      indicator <- pParamIndicator
      basetype <- pBaseType
      ident <- identifier
      return $ Param indicator basetype ident
      
      

pParamIndicator :: Parser ParamIndicator
pParamIndicator
  =   (reserved "val" >> return Val)
  <|> (reserved "ref" >> return Ref)
  <?> "paramindicator"

-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (decls, stmts)




pDecl :: Parser Decl
pDecl
  = do
      basetype <- pBaseType
      identT <- pIdentName
      whiteSpace
      semi
      return $ Decl identT basetype

pBaseType :: Parser BaseType
pBaseType
  = (reserved "bool" >> return BoolType)
    <|>
    (reserved "int" >> return IntType)
    <|>
    (reserved "float" >> return FloatType)
    <?>
    "basetype"


-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIf, pWhile, pCall :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pAsg, pIf, pWhile, pCall]
    <?>
    "statement"

pRead
  = do 
      reserved "read"
      lvalue <- pLvalue
      semi
      return $ Read lvalue

pWrite
  = do 
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return $ Write exp

pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return $ Assign lvalue rvalue

pIf
  = do
      reserved "if"
      cond <- pExp
      reserved "then"
      stmt1 <- many1 pStmt
      try
        (do
          reserved "else"
          stmt2 <- many1 pStmt
          reserved "fi"
          return $ IfElse cond stmt1 stmt2
        )
        <|>
        (do
          reserved "fi"
          return $ If cond stmt1
        )

pWhile
  = do
      reserved "while"
      cond <- pExp
      reserved "do"
      stmt <- many1 pStmt
      reserved "od"
      return $ While cond stmt

pCall
  = do
      reserved "call"
      id <- identifier
      exp <- parens (pExp `sepBy` (symbol ","))
      semi
      return $ Call id exp

pIdentName :: Parser IdName
pIdentName
  = do
      ident <- identifier
      do
        (do
          shape <- brackets (pExp `sepBy1` (symbol ","))
          let l = length shape
          case l > 0 && l <= 2 of
            True -> return $ NameWithShape ident shape
            False -> fail ("Unsupported id dimention of " ++ show l ++ " with " ++ ident ++ show (shape)))
        <|>
        (do
          return $ Name ident)
-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------



pExp, pTerm, pNum, pIdent, pString :: Parser Expr

pExp = buildExpressionParser pOperators pTerm

pOperators = [ [Prefix (reservedOp "-"   >> return (UnaryMinus          ))          ]
              ,[Infix  (reservedOp "*"   >> return (BinExpr Mul        )) AssocLeft,
                Infix  (reservedOp "/"   >> return (BinExpr Div        )) AssocLeft]
              ,[Infix  (reservedOp "+"   >> return (BinExpr Add        )) AssocLeft,
                Infix  (reservedOp "-"   >> return (BinExpr Sub        )) AssocLeft]
              ,[Prefix (reservedOp "!"   >> return (UnaryNot            ))          ]
              ,[Infix  (reservedOp ">="  >> return (BinExpr GreaterEqu )) AssocLeft,
                Infix  (reservedOp "<="  >> return (BinExpr LessEqu    )) AssocLeft,
                Infix  (reservedOp ">"   >> return (BinExpr Greater    )) AssocLeft,
                Infix  (reservedOp "<"   >> return (BinExpr Less       )) AssocLeft,
                Infix  (reservedOp "!="  >> return (BinExpr NotEqu     )) AssocLeft]
              ,[Infix  (reservedOp "&&"  >> return (BinExpr And        )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BinExpr Or         )) AssocLeft]
              ,[Infix  (reservedOp "="   >> return (BinExpr Equ       )) AssocLeft]
              ]

pTerm 
  = parens pExp
  <|> pString
  <|> pNum 
  <|> pIdent
  <|> (reserved "true"  >> return (BoolConst True))
  <|> (reserved "false" >> return (BoolConst False))

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return $ StrConst str
    <?>
    "string"

pNum 
    = do
        ws <- many1 digit
        lexeme (try
                  (do 
                    char '.'
                    ds <- many1 digit
                    let val = read (ws ++ ('.' : ds)) :: Float
                    return $ Num val
                  )
                  <|>
                  (do 
                    return $ IntConst (read ws :: Int)                   
                  )
                )
        <?>
        "Numer"

pIdent
  = do
      identT <- pIdentName
      return $ Id identT
    <?>
    "identifier"


pLvalue :: Parser Lvalue
pLvalue
  = do
      identT <- pIdentName
      return $ LId identT
    <?>
    "lvalue"

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

 
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- pProg
      return p




main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      processArgs progname args


processArgs :: String -> [String] -> IO ()

processArgs progname []
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)

processArgs progname [filename]
  = do
      putStrLn ("Sorry, cannot generate code yet")
      exitWith (ExitFailure 1)

processArgs progname (x:y:xs)
  = do
      let l = length xs
      case x == "-p" && l == 0 of
        True -> do
                  input <- readFile y
                  let output = runParser pMain 0 "" input
                  case output of
                    Right ast -> do
                                  prettyPrinter ast
                    Left  err -> do
                                  putStr "Parse error at "
                                  print err
        False -> do
                  putStrLn ("Unsupported operation or too many arguments")
                  putStrLn ("Usage: " ++ progname ++ " [-p] filename")
                  exitWith (ExitFailure 1)



