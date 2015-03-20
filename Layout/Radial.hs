{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout.Radial
       ( 
        radialLayout
       , renderTree
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
-- Radial Layout Implementation 

radialLayout :: Tree a -> Tree (a,P2)
radialLayout t = unRelativizeRadial (mkP2 1 1) t

unRelativizeRadial :: P2 -> Tree a -> Tree(a,P2)
unRelativizeRadial curPt (Node a ts) = Node (a, curPt) (L.zipWith (\v t -> unRelativizeRadial (curPt .+^ v) t) vs ts)
  where	vs   = L.map (scale (1/len)) (K.iterateN len (rotateBy (1/fromIntegral len)) unit_X)
        len  = fromIntegral (L.length ts)

renderTree :: Monoid' m
           => (a -> QDiagram b R2 m) -> (P2 -> P2 -> QDiagram b R2 m)
           -> Tree (a, P2) -> QDiagram b R2 m
renderTree n e = renderTree' n (e `on` snd)

----------------------------------------------------------------------
-- Tree rendering
--   Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.

renderTree' :: Monoid' m
           => (a -> QDiagram b R2 m) -> ((a,P2) -> (a,P2) -> QDiagram b R2 m)
           -> Tree (a, P2) -> QDiagram b R2 m
renderTree' renderNode renderEdge = centerXY . renderTreeR
  where
    renderTreeR (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (L.map renderTreeR cs)
      <> mconcat (L.map (renderEdge (a,p) . rootLabel) cs)
