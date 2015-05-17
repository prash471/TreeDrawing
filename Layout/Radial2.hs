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

radialLayoutstart:: Tree a -> Double -> Double -> Tree (a,P2,Int)
radialLayoutstart t alpha beta = radialLayout alpha beta alpha (countLeaves (decorateDepth 0 t)) (decorateDepth 0 t)


radialLayout :: Double -> Double -> Double -> Int -> Tree (a,P2,Int) ->  Tree (a, P2, Int)
radialLayout alpha beta theta k (Node (a,pt,d) ts) = Node (a,pt,d) (foo alpha beta theta k ts)

foo :: Double -> Double -> Double -> Int -> [Tree (a,P2,Int)] -> [Tree (a,P2,Int)]
foo alpha beta theta k [] = []
foo alpha beta theta k ((Node (a,pt,d) ts1):ts2)
	= (Node (a,pt2,d) (foo theta u theta lambda ts1)) : foo alpha beta u k ts2	
	where	lambda  = countLeaves (Node (a,pt,d) ts1) 	
		u       = theta + (beta - alpha) * fromIntegral lambda / fromIntegral k
		pt2 	= mkP2 (fromIntegral d * cos (theta + u)/2) (fromIntegral d * sin (theta + u)/2)
		
decorateDepth:: Int -> Tree a -> Tree (a,P2,Int)
decorateDepth d (Node a ts) = Node (a,mkP2 0 0,d) $ L.map (decorateDepth (d+1)) ts

countLeaves :: Tree (a,P2,Int) -> Int 
countLeaves (Node _ []) = 1
countLeaves (Node _ ts) = L.sum (L.map countLeaves ts)

finalTree :: Tree (a,P2,Int) -> Tree (a,P2)
finalTree (Node (a,pt,d) ts) = Node (a,pt) $ L.map finalTree ts
