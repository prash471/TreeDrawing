{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout.Radial
       ( 
        radialLayout1
       ,radialLayout2
       ,renderTree
       ) where

import Data.Tree
import Diagrams.TwoD.Types
import Data.Vector.Unboxed
import Data.List as L
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Transform
import Diagrams.Util as K
import Diagrams.Prelude
import Data.Function       (on)

---------------------------------------------------------
-- Radial Layout Implementation 1: All children on circle centered at parent

radialLayout :: Tree a -> Tree (a,P2)
radialLayout t = unRelativizeRadial1 (mkP2 0 0) t

unRelativizeRadial1 :: P2 -> Tree a -> Tree(a,P2)
unRelativizeRadial1 curPt (Node a ts) = Node (a, curPt) (L.zipWith (\v t -> unRelativizeRadial1 (curPt .+^ v) t) vs ts)
  where	L.map (scale (1/len)) (L.map (rotateBy (1/fromIntegral len/2)) (K.iterateN len (rotateBy (1/fromIntegral len)) unit_X))
        len  = fromIntegral (L.length ts)

---------------------------------------------------------
-- Radial Layout Implementation 2: All children on circle centered at root of the tree in expanding form

foo :: P2 -> P2
foo k = mkP2 (r * cos t) (r * sin t)
  where r   = (-1) * snd (unp2 (k))
        t   = 2 * pi * fst (unp2 (k)) / 64
        -- scale as 64: depending upon tree width

radialLayout2 :: Tree a -> Tree (a,P2)
radialLayout2 t = unRelativizeRadial2 t

unRelativizeRadial2 :: Tree (a,P2) -> Tree(a,P2)
unRelativizeRadial2 tree =  fmap (\x -> (fst x, foo (snd x))) tree

----------------------------------------------------------------------
-- Tree rendering
--   Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.

renderTree :: Monoid' m
           => (a -> QDiagram b R2 m) -> (P2 -> P2 -> QDiagram b R2 m)
           -> Tree (a, P2) -> QDiagram b R2 m
renderTree n e = renderTree' n (e `on` snd)

renderTree' :: Monoid' m
           => (a -> QDiagram b R2 m) -> ((a,P2) -> (a,P2) -> QDiagram b R2 m)
           -> Tree (a, P2) -> QDiagram b R2 m
renderTree' renderNode renderEdge = centerXY . renderTreeR
  where
    renderTreeR (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (L.map renderTreeR cs)
      <> mconcat (L.map (renderEdge (a,p) . rootLabel) cs)
