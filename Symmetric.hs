module Symmetric where

import Algebra
import Data.List

data Perm = Perm [Int]
    deriving(Show, Eq)

compose :: Perm -> Perm -> Perm
compose (Perm as) (Perm bs) 
    |length as == length bs && length as == 6 && sort as == sort bs = Perm $ map fst $ reverse $ sortBy (\b -> \a -> compare (snd a) (snd b)) $ zipWith (,) as bs
    |otherwise = Perm []

instance Group Perm where
    gbinop = compose
    gid = Perm [1,2,3,4,5,6]
    ginv pas = pas


    
    

