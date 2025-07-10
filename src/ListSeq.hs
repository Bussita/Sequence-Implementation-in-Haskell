module ListSeq where

import GHC.Float
import Par
import Seq
import Arr (empty, length)

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS [] = 0
  lengthS (x:xs) = 1 + lengthS xs 

  nthS [] _ = error "Index out of range."  
  nthS (x:xs) n = if n == 0 then x else nthS xs (n-1)

  tabulateS f 0 = [f 0]
  tabulateS f n = tabulateS' f (n - 1) 0
    where
    tabulateS' :: (Int -> a) -> Int -> Int -> [a]
    tabulateS' f n i
            | i == n = [f n]
            | otherwise =
                          let
                            (fx, rest) = f i ||| tabulateS' f n (i + 1)
                          in fx : rest

  mapS _ [] = []
  mapS f (x : xs) = x' : rest
    where
      (x', rest) = f x ||| mapS f xs

  filterS p [] = emptyS 
  filterS p (x : xs) = 
                       let
                         (res, rest) = p x ||| filterS p xs
                       in 
                         if res then x : rest else rest
          

  appendS [] t = t
  appendS (x:xs) t = x : appendS xs t 

  takeS s 0 = emptyS 
  takeS (x:xs) n = x : takeS xs (n-1)

  dropS s 0 = s
  dropS [] _ = emptyS 
  dropS (x : xs) n = dropS xs (n - 1) 

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

  joinS [] = emptyS
  joinS (s:ss) = nthS s 0 : joinS (dropS s 1 : ss) 

  reduceS op e s
    | lengthS s == 0 = e
    | lengthS s == 1 = e `op` (nthS s 0)
    | otherwise = 
        let 
            stack = []
            (_, res) = red (mapS mapFunction s) e stack op
        in res


  scanS op e s
    | lengthS s == 0 = (emptyS, e)
    | lengthS s == 1 = ([e], e `op` (nthS s 0))
    | otherwise =
        let 
            (cont, sLen) = contract s [] op ||| lengthS s
            (s', red) = scanS op e cont
            r = expand s s' 0 sLen op
        in (r, red)
        

  fromList = id


contract :: [a] -> [a] -> (a -> a -> a) -> [a]
contract [] ys _ = ys
contract (x : xs) [] op = contract xs [x] op
contract (x : xs) (y : ys) op =
      let
        (res, rest) = y `op` x ||| contract xs ys op
      in 
        res : rest

expand :: [a] -> [a] -> Int -> Int -> (a -> a -> a) -> [a]
expand s [] _ _ _ = [] -- s' se vacia primero o al mismo tiempo que s', pues tengo length s / 2 techo de elementos
expand st@(x : s) st'@(y : s') i n op
    | i == n = []  -- No quiero operar el ultimo de s, en cambio,
                   -- si el ultimo de s'
    | (i `mod` 2) == 0 = y : expand st st' (i + 1) n op
    | otherwise =
          let
            (res, rest) = y `op` x ||| expand (dropS s 1)  s' (i + 1) n op
          in  
            res : rest
                  
red :: [(Int, a)] -> a -> [(Int, a)] -> (a -> a -> a) -> (Int, a)
red s' e' stack op'
      | lengthS s' == 0 = foldr (combine op') (0, e') stack
      | lengthS s' == 1 = 
        let
          fst = nthS s' 0
        in red emptyS e' (eval (fst : stack) op') op'
      | otherwise = 
        let 
            ((_, fst), (_, snd)) = nthS s' 0 ||| nthS s' 1
            res = (2, fst `op'` snd)

        in case stack of
             [] -> red (dropS s' 2) e' [res] op'
             xs -> 
                   let 
                     evalStack = eval (res : xs) op'
                   in red (dropS s' 2) e' evalStack op'

eval :: [(Int, a)] -> (a -> a -> a) -> [(Int, a)]
eval [] _ = []
eval (x : []) _ = [x]
eval stack@((n1, opRes1) : (n2, opRes2) : xs) op
  | n1 == n2 = eval ((combine op (n1, opRes1) (n2, opRes2)) : xs) op
  | otherwise = stack

-- Opera el el elemento2 con el elemento 1 -> e2 op e1
combine :: (a -> a -> a) -> (Int, a) -> (Int, a) -> (Int, a)
combine op (n1, opRes1) (n2, opRes2) = (n2+n1, opRes2 `op` opRes1)

mapFunction :: a -> (Int, a)
mapFunction x = (1, x)

ilog2 :: Int -> Int
ilog2 n
        | n < 1 = error "ilog2 no definido para n < 1"
        | otherwise = go n 0
  where
    go 1 acumulador = acumulador
    go x acumulador = go (x `div` 2) (acumulador + 1)

fview :: String -> String -> String
fview s0 s1 = " (" ++ s0 ++ " + " ++ s1 ++ ") "
