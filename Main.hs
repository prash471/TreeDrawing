{-# LANGUAGE NoMonomorphismRestriction #-}
import Layout.Radial
import Diagrams.Prelude
import Data.Tree
import Diagrams.Backend.SVG.CmdLine

t1 = Node 'A' [Node 'B' (L.map lf "CDQXXXXXXXX"), Node 'I' (L.map lf "CDQ"), Node 'L' (L.map lf "CDQ")] where lf x = Node x []
--t1 = Node 'A' [Node 'B' (L.map lf "CDQXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"), Node 'I' [Node 'J' (L.map lf "CDQXXXX")]] where lf x = Node x []
--t1 = Node 'A' [Node 'B' [], Node 'C'[], Node 'D'[], Node 'E'[], Node 'F'[], Node 'G'[], Node 'H'[], Node 'I'[] ]

example =
   renderTree (\n -> (text (show n) # fontSizeG 0.5
                            <> circle 0.5 # fc white))
             (~~) (radialLayout t1)
   # centerXY # pad 1.1

main = defaultMain example
