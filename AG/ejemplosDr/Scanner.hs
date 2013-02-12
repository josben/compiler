module Scanner where

{-
 * Autor(es): Pablo Azero (PRA)
 *            Victor Rodriguez (VR)
 * Version  :   2001 PRA
 *            102005 VR
 -}

import Char
import UU.Parsing

---- Analisis sera una lista de simbolos 

type Analisis = [Simbolo]                                   

data Simbolo = Simbolo Tipo_Simbolo String Int

instance Show Simbolo where
  show (Simbolo tipo nom nume) = show tipo ++ " " ++ show nom ++ " en la linea " ++ show nume ++ "\n"

data Tipo_Simbolo  = Entero 
                   | Flotante 
                   | Identificador
                   | Separador 
                   | Operacion 
                   | Reservada 
                   | Comentario1 
                   | Comentario2 
                   | Error
                    deriving (Eq, Ord)

instance Show Tipo_Simbolo where
  show Separador      = " Separadores       : "
  show Reservada      = " Palabra Reservada : "
  show Identificador  = " Identificador     : "
  show Operacion      = " Operador          : "
  show Entero         = " Numero entero     : "
  show Flotante       = " Numero flotante   : "
  show Comentario1    = " Comentario        : "
  show Comentario2    = " Comentario        : "
  show Error          = " Error             : "


reservadas   =["class","int","void","public","private"]
separadores  = ["=","(",")","{","}",",",";"]
operaciones  =['+','-','*','/']

--------------------------------------------------------------------------------------

--mainlex :: String -> IO [Simbolo]
--mainlex file = do leido <- scan file;
--                  return leido;

scan file = do input <- readFile file
               let resul = scanner input 1
               return resul

-------------------- CUERPO PRINCIPAL DEL SCANNER -------------------

scanner  xs n = scanner' xs n
  where scanner' []         _ = []
        scanner' xxs@(x:xs) n = let (tok,str,txs) = token x xs
                                    (nn,     nxs) = saltarBlancos xxs n
                             in  if isSpace x then                     scanner' nxs nn
                                              else Simbolo tok str n : scanner' txs n

        isOperator x     = x `elem` operaciones

        isSpecial  x     = any (==x:[]) separadores

        extractToken p tkSet s n x xs
                         = let (str,xss) = span p xs
                               nxs       = x:str
                           in  ((nxs `elem` tkSet) .?. (s,n), nxs, xss)

        extractToken2 p tkSet s n x xs
                          = ((x:[] `elem` tkSet) .?. (s,n), x:[], xs)

        token x xs
          | isDigit    x =  let (str,xss) = span isDigit xs
                            in  (Entero, x:str, xss)
          | isSpecial  x = extractToken2 isSpecial  separadores Separador Separador  x xs
          | isAlpha    x = extractToken  isAlphaNum reservadas Reservada Identificador  x xs
          | isOperator x = extractToken  isOperator [] Operacion Operacion  x xs
          | otherwise    = (Error, "simbolo desconocido \"" ++ x:[] ++ "\"", xs)

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | otherwise = (n,xxs)

p .?. (esp,normal)
  | p         = esp
  | otherwise = normal

isFloat   :: String -> Bool
isFloat cs = case (dropWhile isDigit cs) of
   ('.':d:cs1) -> isDigit d
   _ -> False

elreal [] = []
elreal (z:zs) | isDigit z = (z):(elreal zs)
              | (z=='.') = (z):(elreal zs)

              ------------------------------ Integracion con el Parser

instance Eq Tipo_Simbolo => Eq Simbolo where
  (Simbolo t1 s1 _) == (Simbolo t2 s2 _)
    = t1 == t2 && (  t1 == Identificador || t1 == Entero  -- No interesa el valor de s1 y s2
                  || s1 == s2                           -- Interesa el valor de s1 y s2
                  )

instance Ord Simbolo where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Simbolo tok1 str1 _ <= Simbolo tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Simbolo

obtenerVal (Simbolo _ v _) = v

tSym :: Tipo_Simbolo -> String -> Parser Simbolo String
tSym tk str  = obtenerVal <$> pSym (Simbolo tk str 0)

pInteger    = aEnt <$> tSym Entero   ""
pReal       = aReal<$> tSym Flotante ""
pVarid      =          tSym Identificador   ""
pClave   pc =          tSym Reservada pc
pOper    op =          tSym Operacion op
pSpec    sp =          tSym Separador sp

aEnt :: String -> Int
aEnt = read

aReal :: String -> Float
aReal = read
