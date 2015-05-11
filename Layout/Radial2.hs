{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Tree
import Diagrams.TwoD.Types
import Data.Vector.Unboxed
import Data.List as L
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Transform
import Diagrams.Util as K
import Diagrams.Prelude
import Data.Function       (on)

t1 = Node 'A' [Node 'B' (L.map lf "CD"), Node 'I' (L.map lf "JK")] where lf x = Node x []

decorateDepth:: Int -> Tree a -> Tree (a,Int)
decorateDepth d (Node a ts) = Node (a,d) $ L.map (decorateDepth (d+1)) ts

radialLayoutstart:: Tree a -> Double -> Double -> Tree (a,P2)
radialLayoutstart t1 alpha beta = radialLayout alpha beta (countLeaves (decorateDepth 0 t1)) (decorateDepth 0 t1)

radialLayout :: Double -> Double -> Int -> Tree (a,Int) ->  Tree (a, P2)
radialLayout alpha beta k (Node (a,d) []) = Node (a,pt) []
	where 	pt 	= mkP2 (fromIntegral d * cos (theta + u)/2) (fromIntegral d * sin (theta + u)/2)
  		u       = theta
        	theta 	= alpha 

radialLayout alpha beta k (Node (a,d) ts) = Node (a,pt) (L.map (radialLayout alpha beta k) ts)   
	where 	pt 	= mkP2 (fromIntegral d * cos (theta + u)/2) (fromIntegral d * sin (theta + u)/2)
  		u       = theta + (beta - alpha) * fromIntegral lambda / fromIntegral k  
        	lambda  = countLeaves (Node (a,d) ts)
        	theta 	= alpha 

countLeaves :: Tree (a,Int) -> Int 
countLeaves t = L.length $ L.last (L.map (L.map rootLabel) $ L.takeWhile (not . L.null) $ iterate (L.concatMap subForest) [t])
