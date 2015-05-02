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

radialLayoutstart:: Tree a -> Angle -> Angle -> Tree (a,P2)
radialLayoutstart t1 alpha beta = radialLayout (decorateDepth t1) alpha beta (countLeaves t1) 

radialLayout :: Tree (a,Int) -> Angle -> Angle -> Int -> Tree (a, P2)
radialLayout (Node (a,d) ts) alpha beta k = Node (a,pt) ts # map (countChildren alpha beta r k) ts   
	where 	pt 	= mkP2 (d * cos (theta + u)/2) (d * sin (theta + u)/2)
  		u       = theta + (beta - alpha) * lambda / k  
        	lambda  = countLeaves t 
        	theta 	= alpha 

countLeaves :: Tree (a,P2) -> Int 
countLeaves t = L.length $ L.last (L.map (L.map rootLabel) $ L.takeWhile (not . L.null) $ iterate (L.concatMap subForest) [t])

countChildren :: Double -> Angle -> Angle -> Tree (a,Int) -> Tree (a,P2)
countChildren alpha beta r k (Node (a,d) ts) k =
	if L.length ts > 0 then  radialLayout (Node (a,d) ts) alpha beta k
	otherwise return  
