-- do not edit; automatically generated by UU.AG
module Arbol where

import List

main = sem_Root ejemplo

ejemplo = Root_Root(Arbol_Rama (Arbol_Rama (Arbol_Hoja 2) 
			(Arbol_Rama (Arbol_Hoja 5) (Arbol_Hoja 20))) (Arbol_Hoja 10))
-- Arbol -------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      attr                 : [Int]

-}
{-
   local variables for Arbol.Hoja:

-}
{-
   local variables for Arbol.Rama:

-}
data Arbol = Arbol_Hoja (Int)
           | Arbol_Rama (Arbol) (Arbol)
-- semantic domain
type T_Arbol = ( ([Int]))
-- cata
sem_Arbol :: (Arbol) ->
             (T_Arbol)
sem_Arbol ((Arbol_Hoja (_int))) =
    (sem_Arbol_Hoja (_int))
sem_Arbol ((Arbol_Rama (_izq) (_der))) =
    (sem_Arbol_Rama ((sem_Arbol (_izq))) ((sem_Arbol (_der))))
sem_Arbol_Hoja :: (Int) ->
                  (T_Arbol)
sem_Arbol_Hoja (int_) =
    let _lhsOattr :: ([Int])
        -- "/home/benjamin/workspace/GenericPortlet/AG/Arbol.ag"(line 25, column 9)
        (_lhsOattr@_) =
            [int_]
    in  ( _lhsOattr)
sem_Arbol_Rama :: (T_Arbol) ->
                  (T_Arbol) ->
                  (T_Arbol)
sem_Arbol_Rama (izq_) (der_) =
    let _lhsOattr :: ([Int])
        _izqIattr :: ([Int])
        _derIattr :: ([Int])
        ( _izqIattr) =
            (izq_ )
        ( _derIattr) =
            (der_ )
        -- "/home/benjamin/workspace/GenericPortlet/AG/Arbol.ag"(line 27, column 9)
        (_lhsOattr@_) =
            (_izqIattr)++(_derIattr)
    in  ( _lhsOattr)
-- Mio ---------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for Mio.Mio:

-}
data Mio = Mio_Mio (Int)
-- semantic domain
type T_Mio = ( )
-- cata
sem_Mio :: (Mio) ->
           (T_Mio)
sem_Mio ((Mio_Mio (_int))) =
    (sem_Mio_Mio (_int))
sem_Mio_Mio :: (Int) ->
               (T_Mio)
sem_Mio_Mio (int_) =
    let 
    in  ( )
-- Root --------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      attr                 : [Int]

-}
{-
   local variables for Root.Root:

-}
data Root = Root_Root (Arbol)
-- semantic domain
type T_Root = ( ([Int]))
-- cata
sem_Root :: (Root) ->
            (T_Root)
sem_Root ((Root_Root (_arbol))) =
    (sem_Root_Root ((sem_Arbol (_arbol))))
sem_Root_Root :: (T_Arbol) ->
                 (T_Root)
sem_Root_Root (arbol_) =
    let _lhsOattr :: ([Int])
        _arbolIattr :: ([Int])
        ( _arbolIattr) =
            (arbol_ )
        -- copy rule (up)
        (_lhsOattr@_) =
            _arbolIattr
    in  ( _lhsOattr)

