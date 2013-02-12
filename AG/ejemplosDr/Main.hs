module Main where

{-
 * Autor(es): Victor Rodriguez (VR)
              Pablo Azero (PRA)
 * Version  : XX102005 VR
              12122005 PRA
 -}

------------------------------------------------------------
import Scanner
import UU.Parsing
-- import AST
import DeclaracionNuevo
-- import AST_GA
------------------------------------------------------------
-- Importando AST             : solamente producir AST
-- main = gcompile (putStr.show) "Ejemplo.java"

-- Importando DeclaracionNuevo: compilacion directa
main = gcompile id "Ejemplo.java"

-- Importando AST_GA          : construcci�n del arbol
--main = gcompile sem_Root "Ejemplo.java"
------------------------------------------------------------
gcompile what file
  = do simbolos <- scan file
       res      <- parseIO pRoot simbolos
       putStr "\n"
       what res
       putStr "\n"
------------------------------------------------------------
pRoot          =   sem_Root_Root      <$> pClase
pClase         =   sem_Programa_Clase <$  pClave "class" <*> pVarid <* pSpec "{" <*> pDeclaraciones <* pSpec "}"

pDeclaraciones =   pFoldr (sem_Decls_Cons, sem_Decls_Nil) pMiembro

pMiembro       =   f_Decl  <$> {- pModificador <*> -} pTipo <*> pVarid <*> pRestoMetodo
  where f_Decl t s m = sem_Decl_Decl (sem_Encab_Encab t s) m
{-  
pModificador   =   sem_Modificador_Public  <$  pClave "public"
               <|> sem_Modificador_Private <$  pClave "private"
-}
pTipo          =   sem_Tipo_Entero  <$  pClave "int"
               <|> sem_Tipo_Nulo    <$  pClave "void"

pRestoMetodo   =   sem_MCuerpo_Just    <$  pParametrosFormales <*> pCuerpoMetodo
               <|> sem_MCuerpo_Nothing <$  pSpec ";"

pParametrosFormales = pSpec "(" <* pSpec ")"

pCuerpoMetodo  =   pSpec "{" *> pCuerpoBloque <* pSpec "}"

pCuerpoBloque  =   pFoldr (sem_Cuerpo_Cons, sem_Cuerpo_Nil) pInstruccion

pInstruccion   = f_Instruccion <$> pVarid <*> pResto <* pSpec ";"
  where f_Instruccion i r = maybe (sem_Llamada_Llamada True i)
                                  (const (sem_Llamada_Llamada False i))
                                  r

pResto         =   pSucceed Nothing
               <|> Just "" <$ pSpec "(" <* pSpec ")"
------------------------------------------------------------
