module SintaxisAbstracta where

data Expr  = Entero         Int
           | Identificador  String
           | Let Decls Expr
           deriving Show

type Decls = [Decl]

data Decl  = Decl String Expr
           deriving Show

sem_Root_Root          e = show e
sem_Expr_Entero          = Entero
sem_Expr_Identificador   = Identificador
sem_Expr_Let             = Let
sem_Decls_Cons           = (:)
sem_Decls_Nil            = []
sem_Decl_Decl            = Decl
