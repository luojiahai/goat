module GoatAST where


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
  | FloatConst Float
  | Id IdName
  | BoolConst Bool
  | UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr
    deriving (Show, Eq)
    
data UnOp
  = Negative | Not
    deriving (Show, Eq)

data BinOp
  = Add | Subtract | Multiply | Divide 
  | And | Or | Equal | Greater | Less 
  | NotEqual | GreaterEqual | LessEqual
    deriving (Show, Eq)

data Decl 
  = Decl IdName BaseType
    deriving (Show, Eq)

data Stmt 
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | IfThen Expr [Stmt]
  | IfThenElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data PrmtIndicator
  = Val 
  | Ref
    deriving (Show, Eq)

data Prmt
  = Prmt PrmtIndicator BaseType Ident
    deriving (Show, Eq)

data Procedure
  = Procedure Ident [Prmt] [Decl] [Stmt]
  | Main [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = GoatProgram [Procedure]
    deriving (Show, Eq)