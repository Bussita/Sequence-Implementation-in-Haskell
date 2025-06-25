module ListSeq where

import GHC.Float
import Par
import Seq

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS s n = s !! n

  tabulateS f n = [f i | i <- [0 .. n - 1]]

  mapS _ [] = []
  mapS f (x : xs) = x' : rest
    where
      (x', rest) = f x ||| mapS f xs

  filterS = filter

  appendS = (++)

  takeS s n = take n s

  dropS s n = drop n s

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

  joinS = reduceS appendS emptyS -- ToDo : revisar

  reduceS op e s
    | lengthS s == 0 = e
    | lengthS s == 1 = nthS s 0
    | otherwise =
        let m = div (lengthS s) 2
            (l', r') = takeS s m ||| dropS s m
         in reduceS op e l' `op` reduceS op e r'

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
