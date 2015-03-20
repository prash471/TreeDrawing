{-# LANGUAGE NoMonomorphismRestriction #-}
import Layout.Radial
import Diagrams.Prelude
import Data.Tree
import Diagrams.Backend.SVG.CmdLine

t1 = Node 'A' [Node 'B' [], Node 'C'[], Node 'D'[], Node 'E'[], Node 'F'[], Node 'G'[], Node 'H'[], Node 'I'[] ]
example =
   renderTree (\n -> (text (show n) # fontSizeG 0.2
                            <> circle 0.03 # fc white))
             (~~) (radialLayout t1)
   # centerXY # pad 1.1

main = defaultMain example
