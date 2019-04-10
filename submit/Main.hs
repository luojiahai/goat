module Main where 

    import GoatAST
    import PrettyPrinter
    import Data.Char
    -- import Debug.Trace
    import Text.Parsec
    import Text.Parsec.Expr
    import Text.Parsec.Language (emptyDef)
    import Control.Monad
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
        , Q.reservedNames     = [ "begin", "bool", "call", "do", "else", "end", "false"
                                , "fi", "float", "if", "int", "od", "proc", "read"
                                , "ref", "then", "true", "val", "while", "write"
                                ]
        , Q.reservedOpNames   = [ "||", "&&", "!", "=", ":=", "!="
                                , "<", "<=", ">", ">=", "+", "-", "*", "/"
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
    strliteral    = Q.stringLiteral lexer
    
    -----------------------------------------------------------------
    -- pProg is the topmost parsing function. It looks for a list of
    -- program headers, e.g. "proc main()", followed by the program body.
    -----------------------------------------------------------------
    pProg :: Parser GoatProgram
    pProg = 
      do
        procs <- many1 pProc
        return $ GoatProgram procs
          
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
            params <- parens (pParam `sepBy` (symbol ","))
            (decls,stmts) <- pProgBody
            return $ Procedure ident params decls stmts
    
    pParam :: Parser Param
    pParam = 
      do
        indicator <- pParamIndicator
        basetype <- pBaseType
        ident <- identifier
        return $ Param indicator basetype ident
          
    pParamIndicator :: Parser ParamIndicator
    pParamIndicator =
      (reserved "val" >> return Val)
      <|> 
      (reserved "ref" >> return Ref)
      <?> 
      "paramindicator"
    
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
          shape <- brackets (pExp `sepBy1` (symbol ","))
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
        exp <- (pString <|> pExp)
        semi
        return $ Write exp
      <?>
      "write"
    
    pAsg = 
      do
        lvalue <- pLvalue
        reservedOp ":="
        rvalue <- pExp
        semi
        return $ Assign lvalue rvalue
      <?>
      "assignment"
    
    pIf = 
      do
        reserved "if"
        cond <- pExp
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
        cond <- pExp
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
        exp <- parens (pExp `sepBy` (symbol ","))
        semi
        return $ Call id exp
      <?>
      "call"
        
    -----------------------------------------------------------------
    --  pExp is the main parser for expressions. It takes into account
    --  the operator precedences and the fact that the binary operators
    --  are left-associative.
    -----------------------------------------------------------------
    pExp, pTerm, pNum, pIdent, pString :: Parser Expr
    
    pExp = buildExpressionParser pOperators pTerm
    
    pOperators = [ [Prefix (reservedOp "-"   >> return (UnaryMinus         ))          ]
                  ,[Infix  (reservedOp "*"   >> return (BinExpr Mul        )) AssocLeft,
                    Infix  (reservedOp "/"   >> return (BinExpr Div        )) AssocLeft]
                  ,[Infix  (reservedOp "+"   >> return (BinExpr Add        )) AssocLeft,
                    Infix  (reservedOp "-"   >> return (BinExpr Sub        )) AssocLeft]
                  ,[Prefix (reservedOp "!"   >> return (UnaryNot           ))          ]
                  ,[Infix  (reservedOp ">="  >> return (BinExpr GreaterEqu )) AssocLeft,
                    Infix  (reservedOp "<="  >> return (BinExpr LessEqu    )) AssocLeft,
                    Infix  (reservedOp ">"   >> return (BinExpr Greater    )) AssocLeft,
                    Infix  (reservedOp "<"   >> return (BinExpr Less       )) AssocLeft,
                    Infix  (reservedOp "!="  >> return (BinExpr NotEqu     )) AssocLeft]
                  ,[Infix  (reservedOp "&&"  >> return (BinExpr And        )) AssocLeft,
                    Infix  (reservedOp "||"  >> return (BinExpr Or         )) AssocLeft]
                  ,[Infix  (reservedOp "="   >> return (BinExpr Equ        )) AssocLeft]
                  ]
    
    pTerm = 
      parens pExp
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
      liftM StrConst strliteral
    
    pNum =
      do
        ws <- many1 digit
        lexeme 
          (try
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
    
    -----------------------------------------------------------------
    -- main
    -----------------------------------------------------------------
    
    pMain :: Parser GoatProgram
    pMain = 
      do
        whiteSpace
        p <- pProg
        return p
    
    main :: IO ()
    main = 
      do
        progname <- getProgName
        args <- getArgs
        processArgs progname args
    
    processArgs :: String -> [String] -> IO ()
    processArgs progname [] = 
      do
        putStrLn ("Usage: " ++ progname ++ " [-p] filename")
        exitWith (ExitFailure 1)
    
    processArgs progname [filename] = 
      do
        putStrLn ("Sorry, cannot generate code yet")
        exitWith (ExitFailure 1)
    
    processArgs progname ["-p",filename] = 
      do
        input <- readFile filename
        let output = runParser pMain 0 "" input
        case output of
          Right ast -> do
                        prettyPrinter ast
                        exitWith ExitSuccess
          Left  err -> do
                        putStr "Parse error at "
                        print err
                        exitWith (ExitFailure 1)
    
    processArgs progname _ = 
      do
        putStrLn ("Unsupported operation or too many arguments")
        putStrLn ("Usage: " ++ progname ++ " [-p] filename")
        exitWith (ExitFailure 1)
    
    
    
    