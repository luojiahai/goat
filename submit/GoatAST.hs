module GoatAST where
    -- AST Part
    
    type Ident = String 
    
    data IdName
      = Name Ident
      | NameWithShape Ident [Expr]
        deriving (Show, Eq)
    
    data BaseType 
      = BoolType | IntType | FloatType
        deriving (Show, Eq)
    
    data Lvalue 
      = LId IdName
        deriving (Show, Eq)
    
    data Expr
      = IntConst Int
      | StrConst String
      | Num Float
      | Id IdName
      | BoolConst Bool
      | UnaryNot Expr
      | UnaryMinus Expr
      | BinExpr BinOp Expr Expr
        deriving (Show, Eq)
        
    data BinOp
      = Equ
      | Greater 
      | GreaterEqu
      | Less
      | LessEqu
      | NotEqu
      | And
      | Or
      | Add
      | Mul
      | Sub
      | Div
        deriving (Show, Eq)
    
    
    data Decl 
      = Decl IdName BaseType
        deriving (Show, Eq)
    
    data Stmt 
      = Assign Lvalue Expr
      | Read Lvalue
      | Write Expr
      | Call Ident [Expr]
      | If Expr [Stmt]
      | IfElse Expr [Stmt] [Stmt]
      | While Expr [Stmt]
        deriving (Show, Eq)
    
    data ParamIndicator
      = Val 
      | Ref
        deriving (Show, Eq)
    
    data Param
      = Param ParamIndicator BaseType Ident
        deriving (Show, Eq)
    
    
    data Procedure
      = Procedure Ident [Param] [Decl] [Stmt]
      | Main [Decl] [Stmt]
        deriving (Show, Eq)
    
    data GoatProgram
      = GoatProgram [Procedure]
        deriving (Show, Eq)