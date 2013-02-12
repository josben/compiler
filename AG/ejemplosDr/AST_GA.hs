module AST_GA ( sem_Root                                  -- Estos nombres se exportan
              , sem_Root_Root
              , sem_Programa_Clase
              , sem_Decls_Cons, sem_Decls_Nil
              , sem_Decl_Decl
              , sem_Encab_Encab
              , sem_Tipo_Entero, sem_Tipo_Nulo
              , sem_MCuerpo_Just, sem_MCuerpo_Nothing
              , sem_Cuerpo_Cons, sem_Cuerpo_Nil
              , sem_Llamada_Llamada
              ) where

{-
 * Autor(es): Pablo Azero (PRA)
 * Version  : 12122005 PRA
 -}

------------------------------------------------------------
import Scanner
import UU.Parsing
import DeclaracionNuevo( sem_Root           -- Del modulo DeclaracionNuevo
                       , Root(..)           -- solamente importamos estos
                       , Programa(..)       -- nombres, el resto está oculto
                       , Decl(..)
                       , Encab(..)
                       , Tipo(..)
                       , Llamada(..)
                       )
------------------------------------------------------------
sem_Root_Root       = Root_Root
sem_Programa_Clase  = Programa_Clase
sem_Decls_Cons      = (:)
sem_Decls_Nil       = []
sem_Decl_Decl       = Decl_Decl
sem_Encab_Encab     = Encab_Encab
sem_Tipo_Entero     = Tipo_Entero
sem_Tipo_Nulo       = Tipo_Nulo
sem_MCuerpo_Just    = Just
sem_MCuerpo_Nothing = Nothing
sem_Cuerpo_Cons     = (:)
sem_Cuerpo_Nil      = []
sem_Llamada_Llamada = Llamada_Llamada
------------------------------------------------------------
