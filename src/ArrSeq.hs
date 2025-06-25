module ArrSeq where

import Arr qualified as A
import Par
import Seq
import Data.List (singleton)

-- import Arr ( (!)) no me funciona el operador y me da error de parseo

instance Seq A.Arr where
  emptyS = A.empty

  fromList = A.fromList

  singletonS x = fromList [x]

  lengthS = A.length

  nthS s i = s A.! i

  takeS s n = A.subArray 0 (n - 1) s

  dropS s n = A.subArray n (lengthS s - n) s

  showtS s
    | lengthS s == 0 = EMPTY
    | lengthS s == 1 = ELT (nthS s 0)
    | otherwise =
        let m = lengthS s `div` 2
            (l, r) = takeS s m ||| dropS s m
         in NODE l r

  showlS s
    | lengthS s == 0 = NIL
    | otherwise =
        let (v, rest) = nthS s 0 ||| dropS s 1
         in CONS v rest

  -- Paralelizamos los show, pero take, drop, y nth son O(1), no cambia la complejidad de la profundidad.

  appendS s t
    | lengthS s == 0 = t
    | lengthS t == 0 = s
    | otherwise = tabulateS (\x -> if x >= lengthS s then nthS t (x - lengthS s) else nthS s x) (lengthS s + lengthS t)

  -- joinS s =
  --   let mid = lengthS s `div` 2
  --       (l, r) =
  --         reduceS appendS emptyS (takeS s mid)
  --           ||| reduceS appendS emptyS (dropS s mid)
  --    in appendS l r

  -- la función appendS tiene profundidad O(1), entonces el maximo de S(x appendS y) para todos x, y en O_r es 1.
  -- Además, al subdividir el problema en dos, tenemos que resolver a lo sumo lg(|s|) casos distintos
  -- por lo tanto S(joinS) = O(lg|s|.1) = O(lg|s|)
  
  joinS = A.flatten

  tabulateS = A.tabulate

  -- Tabulate ya viene implementada en la librería Arr, suponemos que verifica los costos dados.

  mapS f s
    | lengthS s == 0 = emptyS
    | otherwise = appendS v rest
    where
      (v, rest) = singletonS (f (nthS s 0)) ||| mapS f (dropS s 1)

  -- mapS resuelve en paralelo todas las aplicaciones de f(i) y las convierte en singletons, si tenemos que f es O(1), luego
  -- mapS f s = O(max S(f s_i)) = O(1) ya que todas las f s_i son O(1) y luego la máxima de ellas es O(1)

  filterS f s
    | lengthS s == 1 = if f (nthS s 0) then singletonS (nthS s 0) else emptyS
    | otherwise =
        let m = (lengthS s) `div` 2
            (l', r') = filterS f (takeS s m) ||| filterS f (dropS s m)
        in appendS l' r'

  reduceS op e s
      | lengthS s == 0 = e
      | otherwise = e `op` (red op s)
      where
        red op' s' 
                  | lengthS s' == 1 = nthS s' 0
                  | otherwise =
                      let
                          pp = 2 ^ (ilog2 (lengthS s' - 1))
                          (l', r') = reduceS op' e (takeS s' pp) ||| 
                                     reduceS op' e (dropS s' pp)
                          a = showtS s'
                      in l' `op'` r'

fview :: Seq s => s Char -> s Char -> s Char
fview s0 s1 = (fromList " (") `appendS` s0 `appendS` (fromList "+") `appendS` s1 `appendS` (fromList ") ")

ilog2 :: Int -> Int
ilog2 n
        | n < 1 = error "ilog2 no definido para n < 1"
        | otherwise = go n 0
          where
            go 1 acumulador = acumulador
            go x acumulador = go (x `div` 2) (acumulador + 1)


