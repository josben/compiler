module Compilador where

import Scanner
import UU.Parsing
-- import SintaxisAbstracta
import CondicionDeContexto

{- Modulo Compilador de un lenguaje de expresiones
   simple, con abstracciones tipo let.
   Autor  : Pablo R. Azero
   Fecha  : 14 Marzo 2001
   Modificaciones:
   Version 0.2: PRA28082001
      Mejorado para integraci'on con (UU_)Scanner
   Version 0.3: PRA06042006
      Adaptado para usar UU.Parsing.
      Modularizacion de la sintaxis abstracta.
 -}

palabrasClave      = ["in","let"]
operadoresClave    = ["=",";"]
simbolosOperadores = "+-*/=;"

main      = do putStrLn "Compilador 0.3"
               putStr "Archivo: "
               basename <- getLine
               tokens <- scanner palabrasClave
                                 operadoresClave
                                 simbolosOperadores
                                 basename
               resultado <- parseIO pRoot tokens
               putStrLn resultado

pRoot   =   sem_Root_Root <$> pProg

pProg   =   pExpr <|> pLet

pExpr   =   pTerm

pTerm   =   pFactor

pFactor =   sem_Expr_Identificador <$> pIdent
        <|> sem_Expr_Entero        <$> pInt

pLet    =   sem_Expr_Let           <$  pPalClave "let" <*> pDecls
                                   <*  pPalClave "in"  <*> pProg

pDecls  =   pFoldr1Sep (sem_Decls_Cons,sem_Decls_Nil) (pOpClave ";") pDef

pDef    =   sem_Decl_Decl <$> pIdent <* pOpClave "=" <*> pProg
