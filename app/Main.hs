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
      identT <- pIdentType
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


-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIf, pWhile, pCall :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pAsg, pIf, pWhile, pCall]

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

pIdentType :: Parser IdName
pIdentType
  = do
      ident <- identifier
      do
        (do
          shape <- brackets (pExp `sepBy1` (symbol ","))
          case length shape > 0 && length shape <= 2 of
            True -> return $ NameWithShape ident shape
            False -> fail ("Unsupported id dimention of " ++ ident ++ show (shape)))
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
              ,[Infix  (reservedOp "*"   >> return (ABinExpr Mul        )) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinExpr Div        )) AssocLeft]
              ,[Infix  (reservedOp "+"   >> return (ABinExpr Add        )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinExpr Sub        )) AssocLeft]
              ,[Prefix (reservedOp "!"   >> return (UnaryNot            ))          ]
              ,[Infix  (reservedOp ">="  >> return (BBinExpr GreaterEqu )) AssocLeft,
                Infix  (reservedOp "<="  >> return (BBinExpr LessEqu    )) AssocLeft,
                Infix  (reservedOp ">"   >> return (BBinExpr Greater    )) AssocLeft,
                Infix  (reservedOp "<"   >> return (BBinExpr Less       )) AssocLeft,
                Infix  (reservedOp "!="  >> return (BBinExpr NotEqu     )) AssocLeft]
              ,[Infix  (reservedOp "&&"  >> return (BBinExpr And        )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BBinExpr Or         )) AssocLeft]
              ,[Infix  (reservedOp "="   >> return  (BBinExpr Equ       )) AssocLeft]
              ]

pTerm 
  = parens pExp
  <|> (pString)
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
      identT <- pIdentType
      return $ Id identT
    <?>
    "identifier"


pLvalue :: Parser Lvalue
pLvalue
  = do
      identT <- pIdentType
      return $ LId identT
    <?>
    "lvalue"




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
      checkArgs progname args
      input <- readFile (head args)
      let output = runParser pMain 0 "" input
      case output of
        Right ast -> print ast
        Left  err -> do
                      putStr "Parse error at "
                      print err

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
   = return ()
checkArgs progname _
   = do
      putStrLn ("Usage: " ++ progname ++ " filename\n\n")
      exitWith (ExitFailure 1)


