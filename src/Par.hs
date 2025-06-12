{-
 Este módulo requiere la librería parallel. 
 
 Si no la tiene instalada, se puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ cabal update
 $ cabal install parallel
 
-}


module Par ((|||)) where

import Control.Parallel

infix 1 |||

(|||)   ::   a -> b -> (a,b)
a ||| b = a `par` b `par` (a,b)
