module GoatAST where
-- AST Part

type Ident = String

data BaseType 
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)

data BBinOp
  = And | Or
    deriving (Eq,Show)
  
data BExpr 
  = BoolConst Bool 
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp Expr Expr
    deriving(Show)

data Expr
  = IntConst Int
  | StrConst String
  | Num Float
  | Id Ident
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | UnaryMinus Expr
    deriving (Show, Eq)
    

data RBinOp
  = Equ
  | Greater 
  | GreaterEqu
  | Less
  | LessEqu
  | NotEqu
    deriving (Show)

-- data ABinOp 
--   = Add
--   | Sub
--   | Mul
--   | Div
--     deriving (Show)

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

data GoatProgram
  = Program [Decl] [Stmt]
    deriving (Show, Eq)