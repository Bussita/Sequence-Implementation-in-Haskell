module ArrSeq where

import Seq
import Par
import qualified Arr as A
-- import Arr ( (!)) no me funciona el operador y me da error de parseo

instance Seq A.Arr where

  emptyS = A.empty
  
  fromList = A.fromList

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


  appendS s t
    | lengthS s == 0 = t
    | lengthS t == 0 = s
    | otherwise      = tabulateS (\x -> if x >= lengthS s then nthS t (x - lengthS s) else nthS s x) (lengthS s + lengthS t)

  joinS s =
    let mid = lengthS s `div` 2
        (l, r) = reduceS appendS emptyS (takeS s mid)
                 ||| reduceS appendS emptyS (dropS s mid)
    in appendS l r

  tabulateS = A.tabulate

  mapS f s  
    | lengthS s == 0 = emptyS
    | otherwise     = appendS v rest
                        where
                          (v,rest) = singletonS (f (nthS s 0)) ||| mapS f (dropS s 1)
  
  reduceS = undefined

g :: A.Arr a -> A.Arr a -> (Int -> a)
g s t x= if x >= lengthS s then nthS t (x - lengthS s) else nthS s x
