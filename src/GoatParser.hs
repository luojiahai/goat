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


module GoatParser where

import GoatAST
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


-- Lexer
--   Uses Parsec language definition and its lexers
languageDef = 
  emptyDef
    { Token.commentLine       = "#"
    , Token.nestedComments    = True
    , Token.identStart        = letter
    , Token.identLetter       = alphaNum <|> oneOf "_'"
    , Token.opStart           = oneOf "+-*:/|&!<>="
    , Token.opLetter          = oneOf "+-*:/|&!<>="
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
    end <- eof
    return $ GoatProgram procs

-- Parses a procedure
--   This function parses either a main procedure or many other procedures.
pProc :: Parser Procedure
pProc =
  do
    reserved "proc"
    name <- identifier
    prmts <- parens (pPrmt `sepBy` (symbol ","))
    (decls,stmts) <- pProcBody
    return $ Procedure name prmts decls stmts

-- Parses a parameter
pPrmt :: Parser Prmt
pPrmt = 
  do
    indicator <- pPrmtIndicator
    basetype <- pBaseType
    name <- identifier
    return $ Prmt indicator basetype name

-- Parses a parameter indicator
pPrmtIndicator :: Parser PrmtIndicator
pPrmtIndicator =
  (reserved "val" >> return Val)
  <|> 
  (reserved "ref" >> return Ref)
  <?> 
  "prmt indicator"

-- Parses a body of a procedure
--   This function looks for a sequence of declarations followed by a
--   sequence of statements.
pProcBody :: Parser ([Decl],[Stmt])
pProcBody = 
  do
    decls <- many pDecl
    reserved "begin"
    stmts <- many1 pStmt
    reserved "end"
    return (decls, stmts)
  <?> 
  "program body"

-- Parses a declaration
pDecl :: Parser Decl
pDecl = 
  do
    basetype <- pBaseType
    identT <- pIdent
    whiteSpace
    semi
    return $ Decl identT basetype
  <?> 
  "declaration"

-- Parses a base type
pBaseType :: Parser BaseType
pBaseType = 
  (reserved "bool" >> return BoolType)
  <|> 
  (reserved "int" >> return IntType)
  <|> 
  (reserved "float" >> return FloatType)
  <?> 
  "basetype"

-- Parses an identifier
--   This function is for <id>, <id> [<expr>], <id> [<expr>,<expr>].
--   Limits the amount of dimention by measureing the length.
pIdent :: Parser Ident
pIdent = 
  do
    name <- identifier
    do
      shape <- brackets (pExpr `sepBy1` (symbol ","))
      let l = length shape
      case l > 0 && l <= 2 of
        True -> return $ IdentWithShape name shape
        False -> fail ("Unsupported id dimention of " ++ show l ++ " with " ++ name ++ show (shape))
      <|>
      (return $ Ident name)

-- Parses a statement
--   This function is the main parser for statements.
pStmt :: Parser Stmt
pStmt = 
  choice [pRead, pWrite, pAsg, pIf, pWhile, pCall]
  <?> 
  "statement"

-- Parses a/an read/write/assignment/if/while/call statement
pRead, pWrite, pAsg, pIf, pWhile, pCall :: Parser Stmt

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
      return $ IfThenElse cond stmt1 stmt2
      <|>
      do
        reserved "fi"
        return $ IfThen cond stmt1
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

-- Parses an expression
--   This function is the main parser for expressions. It takes into account
--   the operator precedences and the fact that the binary operators
--   are left-associative.
pExpr :: Parser Expr
pExpr = buildExpressionParser pOperators pTerm

pOperators = [ [Prefix (reservedOp "-"   >> return (UnExpr  Negative    ))          ]
              ,[Prefix (reservedOp "!"   >> return (UnExpr Not          ))          ]
              ,[Infix  (reservedOp "*"   >> return (BinExpr Multiply    )) AssocLeft,
                Infix  (reservedOp "/"   >> return (BinExpr Divide      )) AssocLeft]
              ,[Infix  (reservedOp "+"   >> return (BinExpr Add         )) AssocLeft,
                Infix  (reservedOp "-"   >> return (BinExpr Subtract    )) AssocLeft]
              ,[Infix  (reservedOp ">="  >> return (BinExpr GreaterEqual)) AssocLeft,
                Infix  (reservedOp "<="  >> return (BinExpr LessEqual   )) AssocLeft,
                Infix  (reservedOp ">"   >> return (BinExpr Greater     )) AssocLeft,
                Infix  (reservedOp "<"   >> return (BinExpr Less        )) AssocLeft,
                Infix  (reservedOp "!="  >> return (BinExpr NotEqual    )) AssocLeft,
                Infix  (reservedOp "="   >> return (BinExpr Equal       )) AssocLeft]
              ,[Infix  (reservedOp "&&"  >> return (BinExpr And         )) AssocLeft]
              ,[Infix  (reservedOp "||"  >> return (BinExpr Or          )) AssocLeft]
              
              ]

-- Parses a term/num/identifier/string expression
pTerm, pNum, pId, pString :: Parser Expr

pTerm = 
  parens pExpr
  <|> 
  pString
  <|> 
  pNum 
  <|> 
  pId
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

pId = 
  do
    ident <- pIdent
    return $ Id ident
  <?>
  "identifier"

-- Parses a left value of an assignment
pLvalue :: Parser Lvalue
pLvalue = 
  do
    ident <- pIdent
    return $ LId ident
  <?>
  "lvalue"

-- Parses a goat program
pMain :: Parser GoatProgram
pMain = 
  do
    whiteSpace
    p <- pProg
    return p

-- Returns an abstract syntax tree
ast :: String -> Either ParseError GoatProgram
ast = parse pMain ""