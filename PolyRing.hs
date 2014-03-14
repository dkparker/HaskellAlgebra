module PolyRing where

import Algebra
import Data.List

newtype Poly = Poly [Integer]
    deriving(Show, Eq)

addPoly :: Poly -> Poly -> Poly 
addPoly (Poly a) (Poly b)
    | length a < length b = addPoly (Poly (a ++ [0])) (Poly b)
    | length a > length b = addPoly (Poly a) (Poly (b ++ [0]))
    | otherwise = Poly $ zipWith (+) a b

subPoly :: Poly -> Poly -> Poly
subPoly (Poly a) (Poly b)
    | length a < length b = subPoly (Poly (a ++ [0])) (Poly b)
    | length a > length b = subPoly (Poly a) (Poly (b ++ [0]))
    | otherwise = Poly $ zipWith (-) a b

multPoly :: Poly -> Poly -> Poly
multPoly (Poly []) (Poly b) = Poly [] 
multPoly (Poly a) (Poly b) = (Poly $ trunc (fmap (*(last a)) (take (length a - 1) (repeat 0) ++ b))) `addPoly` (multPoly (Poly (init a)) (Poly b)) where
    trunc as
        | as == [] = []
        | last as == 0 = trunc $ init as
        | otherwise = as

instance Ring Poly where
    rplus = addPoly
    rplusid = Poly [] --addPoly adds the necessary amount of zeroes
    rplusinv (Poly a)= Poly $ map (*(-1)) a
    rtimes = multPoly
    rtimesid = Poly [1]

eucFunc :: Poly -> Int
eucFunc (Poly a) = length a --must not have extraneous zeros, usually the case. I trim extra ones from multiplied polynomials and any addition shouldn't have any if the two added polynomials don't have any.
    
