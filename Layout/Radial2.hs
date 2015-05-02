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

unRelativizeRadial3 :: Tree (a,P2) -> Double -> Angle -> Angle -> Tree (a,P2)
unRelativizeRadial3 (Node (a,pt) ts) alpha beta r k = Node (a,pt) (map (foo2 alpha beta r k (Node (a,pt) ts)) ts) # countChildren ts alpha beta r k

foo2:: Double -> Angle -> Angle -> P2 -> P2
foo2 r alpha beta pt t = mkP2 (r * cos (theta + u)/2) (r * sin (theta + u)/2)
  where	u         = theta + (beta - alpha) * lambda / lambda  
        lambda    = countLeaves t 
        theta 	  = alpha

countLeaves :: Tree (a,P2) -> Int 
countLeaves t = L.length $ L.last (L.map (L.map rootLabel) $ L.takeWhile (not . L.null) $ iterate (L.concatMap subForest) [t])

countChildren :: Tree (a,P2) -> Double -> Angle -> Angle -> Tree (a,P2)
countChildren t alpha beta r k =
	if length t > 0 then  unRelativizeRadial3 t alpha beta (r+1) k
	otherwise return  
