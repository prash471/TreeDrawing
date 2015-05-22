{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout.Radial
       ( 
        radialLayout
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

radialLayout :: Tree a -> Tree (a,P2 Double)
radialLayout t = finalTree $ 
		radialLayout' 0 pi 0 (countLeaves $ decorateDepth 0 t) (weight t) (decorateDepth 0 t)

radialLayout' :: Double -> Double -> Double -> Int -> Double -> Tree (a,P2 Double,Int) ->  Tree (a, P2 Double, Int)
radialLayout' alpha beta theta k w (Node (a,pt,d) ts) = Node (a,pt,d) (foo alpha beta theta k w ts)

foo :: Double -> Double -> Double -> Int -> Double  -> [Tree (a,P2 Double,Int)] -> [Tree (a,P2 Double,Int)]
foo alpha beta theta k w [] = []
foo alpha beta theta k w (Node (a,pt,d) ts1:ts2)
		= Node (a,pt2,d) (foo theta u theta lambda w ts1) : foo alpha beta u k w ts2	
	where	
		lambda  = countLeaves (Node (a,pt,d) ts1) 	
		u       = theta + (beta - alpha) * fromIntegral lambda / fromIntegral k
		pt2 	= mkP2 (w * fromIntegral d * cos (theta + u)/2) (w * fromIntegral d * sin (theta + u)/2)

decorateDepth:: Int -> Tree a -> Tree (a,P2 Double,Int)
decorateDepth d (Node a ts) = Node (a,mkP2 0 0,d) $ L.map (decorateDepth (d+1)) ts

countLeaves :: Tree (a,P2 Double,Int) -> Int
countLeaves (Node _ []) = 1
countLeaves (Node _ ts) = L.sum (L.map countLeaves ts)

weight :: Tree a -> Double  
weight t = L.maximum $ 
		  L.map (((\ x -> fromIntegral x / 2) . length) . L.map rootLabel)
		    (takeWhile (not . null) $ iterate (concatMap subForest) [t])

finalTree :: Tree (a,P2 Double,Int) -> Tree (a,P2 Double)
finalTree (Node (a,pt,d) ts) = Node (a,pt) $ L.map finalTree ts

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
