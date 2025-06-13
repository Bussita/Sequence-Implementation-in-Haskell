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

  mapS _ [] = []
  mapS f (x:xs) = f x ||| mapS f xs

  filterS = filter

  appendS = (++)

  takeS s n = take n s

  dropS s n = drop n s

  showtS s
    | lengthS s == 0 = EMPTY
    | lengthS s == 1 = ELT (nthS s 0)
    | otherwise = let m = div (lengthS s) 2
                      (l',r') = takeS s m ||| dropS s m
                  in NODE l' r'


  showlS s
    | lengthS s == 0 = NIL
    | otherwise = let (v,rest) = nthS s 0 ||| dropS s 1
                  in CONS v rest
  
  joinS = reduceS appendS emptyS

  reduceS = foldl'

  scanS op e s = (tabulateS (\i -> reduceS op e (takeS s i)) (lengthS s), reduceS op e s)

  fromList = id