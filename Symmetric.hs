module Symmetric where

import Algebra
import Data.List

data Perm = Perm [Int]
    deriving(Show, Eq)

apply :: Perm -> Perm -> Perm
apply (Perm as) (Perm bs) 
    |length as == length bs && length as == 6 && sort as == sort bs = Perm $ map fst $ reverse $ sortBy (\y -> \x -> compare (snd x) (snd y)) $ zipWith (,) as bs
    |otherwise = Perm []

compose :: Perm -> Perm -> Perm
compose (Perm as) (Perm bs) = apply (apply (Perm [1,2,3,4,5,6]) (Perm bs)) (Perm as)

instance Group Perm where
    gbinop = compose
    gid = Perm [1,2,3,4,5,6]
    ginv pas = pas


    
    

