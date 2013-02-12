
module Scanner where

import UU.Parsing
import Char

{- Modulo Scanner de un lenguaje de expresiones
   simple, con abstracciones tipo let.
   Autor  : Pablo R. Azero
   Fecha  : 14 Marzo 2001
   Modificaciones:
   Version 0.5: PRA28082001
      Mejorado para integraci'on con libreria de combinadores de parsing
 -}

type Tokens = [Token]

data Token  = Token Tok String NumLin
type NumLin = Int

instance Show Token where
  show (Token t str nl) = show t ++ " " ++ show str ++ " en la linea " ++ show nl ++ "\n"

data Tok  = TokIdent
          | TokPalClave
          | TokOpClave
          | TokOperador
          | TokEntero
          | TokCadena
          | TokComentario
          | TokError
          deriving (Eq, Ord)

instance Show Tok where
  show TokPalClave   = " Palabra clave : "
  show TokOpClave    = " Operador clave: "
  show TokIdent      = " Identificador : "
  show TokOperador   = " Operador      : "
  show TokEntero     = " Numero entero : "
  show TokCadena     = " Cadena        : "
  show TokComentario = " Comentario    : "
  show TokError      = " Error         : "

scanner :: PalsClave -> OpsClave -> OpSimbs -> FileName -> IO Tokens
scanner psc osc oss fn = do tokens <- tokenize psc osc oss fn
                            return tokens

type FileName  = String
type PalsClave = [String]
type OpsClave  = [String]
type OpSimbs   = String

tokenize psc osc oss fn = do input <- readFile fn
                             let tokens = scan psc osc oss input 1
                             return tokens

scan psc osc oss xs n = scan' xs n
  where scan' []         _ = []
        scan' xxs@(x:xs) n
          = if isSpace x then scan' nbs nn else Token tok str n : scan' rs n
          where (tok,str,rs) = token x xs
                (nn,nbs)     = saltarBlancos xxs n

        isOperator x       = x `elem` oss

        palClave str
          | str `elem` psc = TokPalClave
          -- | str `elem` psc = if str == "import" then 
          | otherwise      = TokIdent

        opClave str
          | str `elem` osc = TokOpClave
          | otherwise      = TokOperador

        token x cadena@(z:zs)
          | isDigit x    = let (str,xss) = span isDigit    cadena
                           in  (TokEntero   , x:str, xss)
          | isAlpha x    = let (str,xss) = span isAlphaNum cadena
                               nxs       = x:str
                           in  (palClave nxs, nxs  , xss)
          | x == '"'     = let (str,(y:ys)) = span (\a -> a /= '"') cadena
          				   in (TokCadena    , str  , ys)
          | isComment x z = let ((u:us),(y:ys)) = span (\a -> a /= '\n') cadena
          				   in (TokComentario, us  , ys)
          | isOperator x = let (str,xss) = span isOperator cadena
                               nxs       = x:str
                           in  (opClave nxs , nxs  , xss)
          | otherwise    = (TokError, "simbolo desconocido \"" ++ x:[] ++ "\"", cadena)

isComment :: Char -> Char -> Bool
isComment char1 char2 = char1 == char2

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | otherwise = (n,xxs)

-- Integracion con el Parser

instance Eq Tok => Eq Token where
  (Token TokIdent       _  _) == (Token TokIdent       _  _) = True
  (Token TokEntero      _  _) == (Token TokEntero      _  _) = True
  (Token TokCadena      _  _) == (Token TokCadena      _  _) = True
  (Token TokComentario  _  _) == (Token TokComentario  _  _) = True
  (Token t1         s1 _) == (Token t2         s2 _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Token tok1 str1 _ <= Token tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token

obtenerVal (Token _ v _) = v

tSym :: Tok -> String -> Parser Token String
tSym tk str = obtenerVal <$> pSym (Token tk str 0)

pInt         = (\x -> (read x) :: Int) <$> tSym TokEntero   ""
pIdent       =                             tSym TokIdent    ""
pCadena      =                             tSym TokCadena   ""
pPalClave pc =                             tSym TokPalClave pc
pOpClave  oc =                             tSym TokOpClave  oc
