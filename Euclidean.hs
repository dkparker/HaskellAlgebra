module Euclidean where

import Algebra
import Data.List

newtype Poly = [Integer]

addPoly :: Poly -> Poly -> Poly
addPoly a b
    | length a < length b = addPoly (a ++ [0]) b
    | length a > length b = addPoly a (b ++ [0])
    | otherwise = zipWith (+) a b

subPoly :: Poly -> Poly -> Poly
subPoly a b
    | length a < length b = subPoly (a ++ [0]) b
    | length a > length b = subPoly a (b ++ [0])
    | otherwise = zipWith (-) a b

multPoly :: Poly -> Poly -> Poly
multPoly [] b = [] 
multPoly a b = trunc (fmap (*(last a)) (take (length a - 1) (repeat 0) ++ b)) `addPoly` (multPoly ((init a)) b) where
    trunc as
        | as == [] = []
        | last as == 0 = trunc $ init as
        | otherwise = as

--divPoly :: Poly -> Poly -> Poly

instance Ring Poly where
    rplus = addPoly
    rplusid = []
    rplusinv = map (*(-1))
    rtimes = multPoly
    rtimesid = [1]


    
