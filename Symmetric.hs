module Symmetric where

import Algebra
import Data.Array
import Data.Tuple
import Data.List

type Mapping = Array Integer Integer
    
compose :: Mapping -> Mapping -> Mapping
compose a b = array (1,6) mlist where
    mlist = combine (sortBy (\x -> \y -> compare (snd x) (snd y)) (assocs a)) (assocs b)
    combine = zipWith (\x -> \y -> (fst x, snd y)) 

instance Group Mapping where
    gbinop = compose
    gid = listArray (1,6) [1..6]
    ginv a = array (1,6) $ map swap $ assocs a
    

