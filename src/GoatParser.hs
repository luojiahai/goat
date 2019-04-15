module GoatParser where

import GoatAST
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment
import System.Exit

-- Lexer
languageDef = 
  emptyDef
    { Token.commentLine       = "#"
    , Token.nestedComments    = True
    , Token.identStart        = letter
    , Token.identLetter       = alphaNum
    , Token.opStart           = oneOf "+-*:/|&!<>"
    , Token.opLetter          = oneOf "+-*:/|&!<>"
    , Token.reservedNames     = [ "begin", "bool", "call", "do", "else", "end", "false"
                                , "fi", "float", "if", "int", "od", "proc", "read"
                                , "ref", "then", "true", "val", "while", "write"
                                ]
    , Token.reservedOpNames   = [ "||", "&&", "!", "=", ":=", "!="
                                , "<", "<=", ">", ">=", "+", "-", "*", "/"
                                ]
    }

lexer = Token.makeTokenParser languageDef

whiteSpace    = Token.whiteSpace lexer
lexeme        = Token.lexeme lexer
natural       = Token.natural lexer
identifier    = Token.identifier lexer
colon         = Token.colon lexer
semi          = Token.semi lexer
comma         = Token.comma lexer
parens        = Token.parens lexer
brackets      = Token.brackets lexer
reserved      = Token.reserved lexer
reservedOp    = Token.reservedOp lexer
symbol        = Token.symbol lexer
stringLiteral = Token.stringLiteral lexer

-- Parses a goat grogram
--   This function is the topmost parsing function.
--   It looks for a list of program headers, e.g. "proc main()",
--   followed by the program body.
pProg :: Parser GoatProgram
pProg = 
  do
    procs <- many1 pProc
    return $ GoatProgram procs

-- Parses a procedure
--   This function parses either a main procedure or many other procedures.
pProc :: Parser Procedure
pProc =
  do
    reserved "proc"
    do
      reserved "main"
      parens (return ())
      (decls,stmts) <- pProgBody
      return $ Main decls stmts
      <|>
      do
        ident <- identifier
        prmts <- parens (pPrmt `sepBy` (symbol ","))
        (decls,stmts) <- pProgBody
        return $ Procedure ident prmts decls stmts

-- Parses a paramteter
pPrmt :: Parser Prmt
pPrmt = 
  do
    indicator <- pPrmtIndicator
    basetype <- pBaseType
    ident <- identifier
    return $ Prmt indicator basetype ident

-- Parses a prmteter indicator
pPrmtIndicator :: Parser PrmtIndicator
pPrmtIndicator =
  (reserved "val" >> return Val)
  <|> 
  (reserved "ref" >> return Ref)
  <?> 
  "prmt indicator"

-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody = 
  do
    decls <- many pDecl
    reserved "begin"
    stmts <- many1 pStmt
    reserved "end"
    return (decls, stmts)
  <?> 
  "program body"

pDecl :: Parser Decl
pDecl = 
  do
    basetype <- pBaseType
    identT <- pIdentName
    whiteSpace
    semi
    return $ Decl identT basetype
  <?> 
  "declaration"

pBaseType :: Parser BaseType
pBaseType = 
  (reserved "bool" >> return BoolType)
  <|> 
  (reserved "int" >> return IntType)
  <|> 
  (reserved "float" >> return FloatType)
  <?> 
  "basetype"

-- Pariser for <id>, <id> [<expr>], <id> [<expr>,<expr>]
-- Limit the amount of dimention by measureing the length.
pIdentName :: Parser IdName
pIdentName = 
  do
    ident <- identifier
    do
      shape <- brackets (pExpr `sepBy1` (symbol ","))
      let l = length shape
      case l > 0 && l <= 2 of
        True -> return $ NameWithShape ident shape
        False -> fail ("Unsupported id dimention of " ++ show l ++ " with " ++ ident ++ show (shape))
      <|>
      (return $ Name ident)


-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIf, pWhile, pCall :: Parser Stmt

pStmt = 
  choice [pRead, pWrite, pAsg, pIf, pWhile, pCall]
  <?> 
  "statement"

pRead = 
  do 
    reserved "read"
    lvalue <- pLvalue
    semi
    return $ Read lvalue
  <?>
  "read"

pWrite = 
  do 
    reserved "write"
    exp <- (pString <|> pExpr)
    semi
    return $ Write exp
  <?>
  "write"

pAsg = 
  do
    lvalue <- pLvalue
    reservedOp ":="
    rvalue <- pExpr
    semi
    return $ Assign lvalue rvalue
  <?>
  "assignment"

pIf = 
  do
    reserved "if"
    cond <- pExpr
    reserved "then"
    stmt1 <- many1 pStmt
    do
      reserved "else"
      stmt2 <- many1 pStmt
      reserved "fi"
      return $ IfElse cond stmt1 stmt2
      <|>
      do
        reserved "fi"
        return $ If cond stmt1
  <?>
  "if"

pWhile = 
  do
    reserved "while"
    cond <- pExpr
    reserved "do"
    stmt <- many1 pStmt
    reserved "od"
    return $ While cond stmt
  <?>
  "while"

pCall = 
  do
    reserved "call"
    id <- identifier
    exp <- parens (pExpr `sepBy` (symbol ","))
    semi
    return $ Call id exp
  <?>
  "call"
    
-----------------------------------------------------------------
--  pExpr is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------
pExpr, pTerm, pNum, pIdent, pString :: Parser Expr

pExpr = buildExpressionParser pOperators pTerm

pOperators = [ [Prefix (reservedOp "-"   >> return (UnExpr Negative     ))          ]
              ,[Infix  (reservedOp "*"   >> return (BinExpr Multiply    )) AssocLeft,
                Infix  (reservedOp "/"   >> return (BinExpr Divide      )) AssocLeft]
              ,[Infix  (reservedOp "+"   >> return (BinExpr Add         )) AssocLeft,
                Infix  (reservedOp "-"   >> return (BinExpr Subtract    )) AssocLeft]
              ,[Prefix (reservedOp "!"   >> return (UnExpr Not          ))          ]
              ,[Infix  (reservedOp ">="  >> return (BinExpr GreaterEqual)) AssocLeft,
                Infix  (reservedOp "<="  >> return (BinExpr LessEqual   )) AssocLeft,
                Infix  (reservedOp ">"   >> return (BinExpr Greater     )) AssocLeft,
                Infix  (reservedOp "<"   >> return (BinExpr Less        )) AssocLeft,
                Infix  (reservedOp "!="  >> return (BinExpr NotEqual    )) AssocLeft]
              ,[Infix  (reservedOp "&&"  >> return (BinExpr And         )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BinExpr Or          )) AssocLeft]
              ,[Infix  (reservedOp "="   >> return (BinExpr Equal       )) AssocLeft]
              ]

pTerm = 
  parens pExpr
  <|> 
  pString
  <|> 
  pNum 
  <|> 
  pIdent
  <|> 
  pBool
  <?>
  "term" 

pBool = 
  (reserved "true"  >> return (BoolConst True))
  <|>
  (reserved "false" >> return (BoolConst False))

pString = 
  liftM StrConst stringLiteral

pNum =
  do
    ws <- many1 digit
    lexeme 
      (try
        (do 
          char '.'
          ds <- many1 digit
          let val = read (ws ++ ('.' : ds)) :: Float
          return $ FloatConst val
        )
        <|>
        (do 
          let val = read ws :: Int
          return $ IntConst val
        )
      )
    <?>
    "number"

pIdent = 
  do
    identT <- pIdentName
    return $ Id identT
  <?>
  "identifier"


pLvalue :: Parser Lvalue
pLvalue = 
  do
    identT <- pIdentName
    return $ LId identT
  <?>
  "lvalue"

pMain :: Parser GoatProgram
pMain = 
  do
    whiteSpace
    p <- pProg
    return p

ast :: String -> Either ParseError GoatProgram
ast = parse pMain ""