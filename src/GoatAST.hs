module GoatAST where
-- AST Part

type Ident = String

data BaseType 
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)

data Expr
  = IntConst Int
  | StrConst String
  | Num Float
  | Id Ident
  | BoolConst Bool
  | UnaryNot Expr
  | UnaryMinus Expr
  | ABinExpr ABinOp Expr Expr
  | BBinExpr BinOp Expr Expr
    deriving (Show, Eq)
    
data ABinOp
  = Add
  | Mul
  | Sub
  | Div
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
    deriving (Show, Eq)


data Decl 
  = Decl Ident BaseType
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