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


module GoatAST where

type Name = String 

data Ident
  = Ident Name
  | IdentWithShape Name [Expr]
    deriving (Show, Eq)

data BaseType 
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)

data Expr
  = IntConst Int
  | StrConst String
  | FloatConst Float
  | Id Ident
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
  = Decl Ident BaseType
    deriving (Show, Eq)

data Stmt 
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Name [Expr]
  | IfThen Expr [Stmt]
  | IfThenElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data PrmtIndicator
  = Val 
  | Ref
    deriving (Show, Eq)

data Prmt
  = Prmt PrmtIndicator BaseType Name
    deriving (Show, Eq)

data Procedure
  = Procedure Name [Prmt] [Decl] [Stmt]
  | Main [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = GoatProgram [Procedure]
    deriving (Show, Eq)