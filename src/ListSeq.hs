module ListSeq where

import GHC.Float
import Par
import Seq
import Arr (empty, length)

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS empty = 0
  lengthS (x:xs) = 1 + lengthS xs 

  nthS empty _ = error "Index out of range."  
  nthS (x:xs) n = if n == 0 then x else nthS xs (n-1)

  tabulateS f n = [f i | i <- [0 .. n - 1]]

  mapS _ [] = []
  mapS f (x : xs) = x' : rest
    where
      (x', rest) = f x ||| mapS f xs

  filterS p empty = empty
  filterS p xs = [x | p x, x <- xs] 

  appendS empty t = t
  appendS (x:xs) t = x : appendS xs t 

  takeS s 0 = empty
  takeS (x:xs) n = x : takeS xs (n-1)

  dropS s 0 = s
  dropS empty _ = empty
  dropS (x:xs) n = dropS xs (n-1) 

  showtS s
    | lengthS s == 0 = EMPTY
    | lengthS s == 1 = ELT (nthS s 0)
    | otherwise =
        let m = div (lengthS s) 2
            (l', r') = takeS s m ||| dropS s m
         in NODE l' r'

  showlS s
    | lengthS s == 0 = NIL
    | otherwise =
        let (v, rest) = nthS s 0 ||| dropS s 1
         in CONS v rest

  -- joinS = reduceS appendS emptyS -- ToDo : revisar

  joinS empty = empty
  joinS (s:ss) = nthS s 0 : joinS (dropS 1 s : ss) 

-- Arreglar, hacer particion PP
  reduceS op e s
    | lengthS s == 0 = e
    | lengthS s == 1 = nthS s 0
    | otherwise =
      e
      where
          ilog2 :: Int -> Int
          ilog2 n
            | n < 1 = error "ilog2 no definido para n < 1"
            | otherwise = go n 0
            where
              go 1 acumulador = acumulador
              go x acumulador = go (x `div` 2) (acumulador + 1)

  scanS op e s
    | lengthS s == 0 = ([], e)
    | lengthS s == 1 = ([e], e `op` (s !! 0))
    | otherwise =
        let cont :: [a]
            cont = [ (s !! (2*i)) `op` (s !! (2*i+1)) | i <- [0 .. (length s `div` 2) - 1] ]
            (s', red) = scanS op e cont
            r :: [a]
            r = [ if even i
                    then s' !! (i `div` 2)
                    else (s' !! (i `div` 2)) `op` (s !! (i-1))
                | i <- [0 .. length s - 1] ]
        in (r, red)

  fromList = id
