module ArrSeq where

import Seq
import Par
import qualified Arr as A
import qualified Arr as A
import Arr (empty)
import Seq (Seq(emptyS, nthS))
-- import Arr ( (!)) no me funciona el operador y me da error de parseo

instance Seq Arr where

  emptyS = A.empty

  singletonS x = A.fromList [x]

  lengthS = A.length

  nthS s i = s A.! i

  takeS s n = A.subArray 0 (n - 1) s

  dropS s n = A.subArray n (lengthS s - n) s

  showtS s
    | lengthS s == 0 = EMPTY
    | lengthS s == 1 = ELT (nthS s 0)
    | otherwise      =
        let m = lengthS s `div` 2
            (l, r) = takeS s m ||| dropS s m
        in NODE l r

  -- Paralelizamos, pero take y drop son O(1), no cambia la complejidad.
  showlS s
    | lengthS s == 0 = NIL
    | otherwise      =
        let (v, rest) = nthS s 0 ||| dropS s 1
        in CONS v rest

  {-
  appendS s@(x:s1) t@(y:s2)
    | lengthS s == 0 = t
    | lengthS t == 0 = s
    | otherwise      =
        let (prim, rest, ult) = singletonS (nthS s 0)
                              ||| appendS (dropS s 1) (takeS t (lengthS t - 1))
                              ||| singletonS (nthS t (lengthS t - 1))
        in fromListS ([prim] ++ [rest] ++ [ult])  -- <- Esto no compila, hay que completarlo bien
  -}

  joinS s =
    let mid = lengthS s `div` 2
        (l, r) = reduceS appendS emptyS (takeS s mid)
                 ||| reduceS appendS emptyS (dropS s mid)
    in appendS l r

  tabulateS = A.tabulate

  mapS f s  
    | lengtS s == 0 = emptyS
    | otherwise     = appendS v rest
                        where
                          (v,rest) = singletonS (f (nthS s 0)) ||| mapS f (dropS s 1)
  
  reduceS = undefined