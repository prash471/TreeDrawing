{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout.Radial
       ( 
        radialLayout
        genRadialLayout
--       ,radialLayout1
--       ,radialLayout2
       ,renderTree
       ) where

import Data.Tree
import Diagrams.TwoD.Types
--import Data.Vector.Unboxed
import Data.List as L
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Transform
import Diagrams.Util as K
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Data.Function       (on)

--------------------------------------------------------------------
-- Radial Layout Implementation 1 (annulus wedge method)
--
-- alpha beta defines annulus wedge of a vertex
-- d is the depth of any vertex from root
-- k is #leaves of root and lambda is #leaves of vertex
-- weight assigns the length of radius wrt the number of
-- number of children to avoid node overlapping
-- Algotihm 1, Page 18 http://www.cs.cmu.edu/~pavlo/static/papers/APavloThesis032006.pdf
-- Example: https://drive.google.com/file/d/0B3el1oMKFsOIVGVRYzJzWGwzWDA/view
-------------------------------------------------------------------

radialLayout :: Tree a -> Tree (a, P2 Double)
radialLayout t = finalTree $ 
		radialLayout' 0 pi 0 (countLeaves $ decorateDepth 0 t) (weight t) (decorateDepth 0 t)

radialLayout' :: Double -> Double -> Double -> Int -> Double -> Tree (a, P2 Double, Int) ->  Tree (a, P2 Double, Int)
radialLayout' alpha beta theta k w (Node (a, pt, d) ts) = Node (a, pt, d) (assignPos alpha beta theta k w ts)

assignPos :: Double -> Double -> Double -> Int -> Double  -> [Tree (a, P2 Double, Int)] -> [Tree (a, P2 Double, Int)]
assignPos alpha beta theta k w [] = []
assignPos alpha beta theta k w (Node (a, pt, d) ts1:ts2)
		= Node (a, pt2, d) (assignPos theta u theta lambda w ts1) : assignPos alpha beta u k w ts2	
	where	
		lambda  = countLeaves (Node (a, pt, d) ts1) 	
		u       = theta + (beta - alpha) * fromIntegral lambda / fromIntegral k
		pt2 	= mkP2 (w * fromIntegral d * cos (theta + u)/2) (w * fromIntegral d * sin (theta + u)/2)

decorateDepth:: Int -> Tree a -> Tree (a, P2 Double, Int)
decorateDepth d (Node a ts) = Node (a, mkP2 0 0, d) $ L.map (decorateDepth (d+1)) ts

countLeaves :: Tree (a, P2 Double, Int) -> Int
countLeaves (Node _ []) = 1
countLeaves (Node _ ts) = L.sum (L.map countLeaves ts)

weight :: Tree a -> Double  
weight t = L.maximum $ 
		  L.map (((\ x -> fromIntegral x / 2) . length) . L.map rootLabel)
		    (takeWhile (not . null) $ iterate (concatMap subForest) [t])

finalTree :: Tree (a, P2 Double, Int) -> Tree (a, P2 Double)
finalTree (Node (a, pt, d) ts) = Node (a, pt) $ L.map finalTree ts


---------------------------------------------------------
-- Generalized Radial Layout Implementation
--
-- Nodes which are larger in shape will get 
-- more annulus wedge as they need to occupy 
-- more space, Ratio depends upon the node's 
-- diameter to the circumference of the level circle
--------------------------------------------------------------------
-- Radial Layout Implementation 1 (annulus wedge method)
--
-- alpha beta defines annulus wedge of a vertex
-- d is the depth of any vertex from root
-- k is #leaves of root and lambda is #leaves of vertex
-- weight assigns the length of radius wrt the number of
-- number of children to avoid node overlapping
-- Extension of Algotihm 1, Page 18 http://www.cs.cmu.edu/~pavlo/static/papers/APavloThesis032006.pdf
-- Example: https://drive.google.com/file/d/0B3el1oMKFsOIVGVRYzJzWGwzWDA/view
-------------------------------------------------------------------

genRadialLayout :: Tree (QDiagram SVG V2 Double Any) -> Tree (QDiagram SVG V2 Double Any, P2 Double)
genRadialLayout t = finalTree $ 
		radialLayout' 0 pi 0 (countLeaves $ decorateDepth 0 t) (sumRadLevel ((showlevels (decorateDepth 0 t))!!1)) (decorateDepth 0 t)

genRadialLayout' :: Double -> Double -> Double -> Int -> Double -> Tree (QDiagram SVG V2 Double Any, P2 Double, Int) ->  Tree (QDiagram SVG V2 Double Any, P2 Double, Int)
genRradialLayout' alpha beta theta k t (Node (a, pt, d) ts) = Node (a, pt, d) (assignPos' alpha beta theta k t ts)

assignPos' :: Double -> Double -> Double -> Int -> Double -> [Tree (QDiagram SVG V2 Double Any, P2 Double, Int)] -> [Tree (QDiagram SVG V2 Double Any, P2 Double, Int)]
assignPos' alpha beta theta k t [] = []
assignPos' alpha beta theta k t (Node (a, pt, d) ts1:ts2)
		= Node (a, pt2, d) (assignPos' theta u theta lambda phi ts1) : assignPos' alpha beta u k t ts2	
	where	
		lambda  = countLeaves (Node (a, pt, d) ts1)
		--phi	= sumRad (Node (a, pt, d) ts1)
		phi	= sumRadLevel ((showlevels (Node (a, pt, d) ts1))!!d)
		nodeRad	= 2 * findrad 1.0 1.0 a 
		u       = theta + (beta - alpha) * nodeRad / t
		pt2 	= mkP2 ( 10*t*enRad * fromIntegral d *cos (theta + u)/2) ( 10*t*enRad * fromIntegral d *sin (theta + u)/2)
	--	p	= (calcRadius ((beta - alpha) /fromIntegral k) enRad) * fromIntegral d 


sumRad :: Tree (QDiagram SVG V2 Double Any, P2 Double, Int) -> Double
sumRad (Node (a,pt,d) []) = findrad 1.0 1.0 a
sumRad (Node _ ts) = L.sum (L.map sumRad ts)

enRad = maximum $ liftM3 (findrad) [-0.5,-0.4,-0.3,-0.2,-0.1,0.1,0.2,0.3,0.4,0.5] [-0.5,-0.4,-0.3,-0.2,-0.1,0.1,0.2,0.3,0.4,0.5] [d1,d3,d8]

findrad :: Double -> Double -> QDiagram SVG V2 Double Any -> Double
findrad a b d = radius (V2 a b) d

mapTuple (a, b, c) = a

showlevels :: Tree (QDiagram SVG V2 Double Any, P2 Double, Int) -> [[(QDiagram SVG V2 Double Any, P2 Double, Int)]]
showlevels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

sumRadLevel :: [(QDiagram SVG V2 Double Any, P2 Double, Int)] -> Double
sumRadLevel t = 2 * (sum $ map (findrad 1.0 1.0) (map mapTuple t)) 

calcRadius :: Double -> Double -> Double
calcRadius theta enrad = enrad / sin (theta / 2)  

--d1 :: Path V2 Double
d1 :: QDiagram SVG V2 Double Any
--d1 = text "A" # fc black # fontSizeG 6 `atop` circle 11 # fc white
d1 = circle 0.1 # fc white

d8 :: QDiagram SVG V2 Double Any
--d1 = text "A" # fc black # fontSizeG 6 `atop` circle 11 # fc white
d8 = circle 0.4 # fc white

d2 :: QDiagram SVG V2 Double Any
d2 = text "B" # fc black # fontSizeG 6 `atop` rect 70 140 # fc white

d3 :: QDiagram SVG V2 Double Any
d3 = square 0.4 # fc white

d4 :: QDiagram SVG V2 Double Any
d4 = text "D" # fc black # fontSizeG 6 `atop` eqTriangle 13 # fc white

{-
---------------------------------------------------------
-- Radial Layout Implementation 1: 
--
-- All children on circle centered at parent
---------------------------------------------------------

radialLayout1 :: Tree a -> Tree (a,P2 Double)
radialLayout1 t = unRelativizeRadial1 (mkP2 0 0) t

unRelativizeRadial1 :: P2 Double -> Tree a -> Tree(a,P2 Double)
unRelativizeRadial1 curPt (Node a ts) = Node (a, curPt) (L.zipWith (\v t -> unRelativizeRadial1 (curPt .+^ v) t) vs ts)
  where	vs   = L.map (scale (1/len)) (L.map (rotateBy (1/fromIntegral len/2)) (K.iterateN len (rotateBy (1/fromIntegral len)) unit_X))
        len  = fromIntegral (L.length ts)



---------------------------------------------------------
-- Radial Layout Implementation 2: 
--
-- All children on circle centered at root of the tree in expanding form
---------------------------------------------------------

foo2 :: P2 Double -> P2 Double
foo2 k = mkP2 (r * cos t) (r * sin t)
  where r   = (-1) * snd (unp2 (k))
        t   = 2 * pi * fst (unp2 (k)) / 64
        -- scale as 64: depending upon tree width

radialLayout2 :: Tree a -> Tree (a,P2 Double)
radialLayout2 t = unRelativizeRadial2 (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)

unRelativizeRadial2 :: Tree (a,P2 Double) -> Tree(a,P2 Double)
unRelativizeRadial2 tree =  fmap (\x -> (fst x, foo2 (snd x))) tree



------------------------------------------------------------
--  Rendering
------------------------------------------------------------

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.
renderTree :: (Monoid' m, Floating n, Ord n)
           => (a -> QDiagram b V2 n m) -> (P2 n -> P2 n -> QDiagram b V2 n m)
           -> Tree (a, P2 n) -> QDiagram b V2 n m
renderTree n e = renderTree' n (e `on` snd)

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.  Unlike 'renderTree',
--   this version gives the edge-drawing function access to the actual
--   values stored at the nodes rather than just their positions.
renderTree' :: (Monoid' m, Floating n, Ord n)
           => (a -> QDiagram b V2 n m) -> ((a,P2 n) -> (a,P2 n) -> QDiagram b V2 n m)
           -> Tree (a, P2 n) -> QDiagram b V2 n m
renderTree' renderNode renderEdge = alignT . centerX . renderTreeR
  where
    renderTreeR (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (L.map renderTreeR cs)
      <> mconcat (L.map (renderEdge (a,p) . rootLabel) cs)
-}
