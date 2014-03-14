module Elliptic where

import Algebra
import Data.Maybe

curveA = 1
curveB = (-1)
--y^2 = x^3 - Ax - B
--suggested integer points : (1,1) (5,11) (3,5)

data Point = Point Double Double | Zero
    deriving (Show,Eq)

xCoor :: Point -> Maybe Double
xCoor Zero = Nothing
xCoor (Point a b) = Just a

yCoor :: Point -> Maybe Double
yCoor Zero = Nothing
yCoor (Point a b) = Just b

getPosPoint :: Double -> Point
getPosPoint = \x -> Point x $ sqrt $ x^3 - curveA*x - curveB

getNegPoint :: Double -> Point
getNegPoint = \x -> Point x $ ((-1)*) $ sqrt $ x^3 - curveA*x - curveB

addPoints :: Point -> Point -> Point
addPoints  p1 p2 
    | p1 == Zero = p2
    | p2 == Zero = p1
    | x1 /= x2 = let s = (y1 - y2)/(x1 - x2) 
                     x3 = s^2 - x1 - x2
                 in 
                    Point x3 $ (-1)*(y1 + s*(x3 - x1))
    | y1 == (-y2) = Zero
    | otherwise = let s = (3*(x1^2) - curveA)/(2*y1)
                      x3 = s^2 - 2 * x1 
                  in 
                     Point x3 $ (-1)*(y1 + s*(x3 - x1))
    where
        x1 = fromJust $ xCoor p1
        y1 = fromJust $ yCoor p1
        x2 = fromJust $ xCoor p2
        y2 = fromJust $ yCoor p2

instance Group Point where
    gbinop = addPoints
    gid = Zero
    ginv (Point a b) = Point a (-b) 
