module ListSeq where

import GHC.Float
import Par
import Seq
import Seq (TreeView (EMPTY))
import Data.Foldable (Foldable(foldl'))

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS s n = s !! n

  tabulateS f n = [f i | i <- [0 .. n - 1]]

  mapS = map

  filterS = filter

  appendS = (++)

  takeS s n = take n s

  dropS s n = drop n s

  showtS s
    | lengthS s == 0 = EMPTY
    | lengthS s == 1 = ELT (nthS s 0)
    | otherwise = NODE (takeS s (div (lengthS s) 2)) (dropS s (div (lengthS s) 2))

  showlS s
    | lengthS s == 0 = NIL
    | otherwise = CONS (nthS s 0) (dropS s 1)
  
  joinS = reduceS appendS emptyS

  reduceS = foldl'

  scanS op e s = (tabulateS (\i -> reduceS op e (takeS s i)) (lengthS s), reduceS op e s)

  fromList = id