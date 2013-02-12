module AST where

{-
 * Autor(es): Victor Rodriguez (VR)
              Pablo Azero (PRA)
 * Version  : XX102005 VR
              12122005 PRA
 -}

-------------------------------------------------------------
data Root          = Root Programa
                   deriving Show

data Programa      = Clase String Declaraciones
                   deriving Show

data Declaraciones = NilDecls
                   | ConsDecls Decl Declaraciones
                   deriving Show

data Decl          = Decl {- Modificador -} Tipo String RestoMetodo
                   deriving Show

data Modificador   = Public
                   | Private
                   deriving Show

data Tipo          = TEntero
                   | TNulo
                   deriving Show

data RestoMetodo   = Metodo   CuerpoMetodo
                   | Atributo
                   deriving Show

data CuerpoMetodo  = NilMetodo
                   | ConsMetodo Instruccion CuerpoMetodo
                    deriving Show

data Instruccion   = IVariable String
                   | IMetodo String
                   deriving Show
-------------------------------------------------------------
sem_Root_Root             = Root
sem_Programa_Clase        = Clase
sem_Decls_Nil             = NilDecls
sem_Decls_Cons            = ConsDecls
sem_Decl_Decl (a,b) r     = Decl a b r 
sem_Encab_Encab a b       = (a,b)
sem_Modificador_Public    = Public
sem_Modificador_Private   = Private
sem_Tipo_Entero           = TEntero
sem_Tipo_Nulo             = TNulo
sem_MCuerpo_Just           = Metodo
sem_MCuerpo_Nothing        = Atributo
sem_Cuerpo_Nil            = NilMetodo
sem_Cuerpo_Cons           = ConsMetodo
sem_Llamada_Llamada True  = IVariable
sem_Llamada_Llamada False = IMetodo
