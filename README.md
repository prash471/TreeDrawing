# TreeDrawing

Execution

1) cabal build

2) cabal exec ghc Main.hs

3) ./Main -o *.svg -w 400

Comparison with Symmetric Layout https://drive.google.com/file/d/0B3el1oMKFsOIVGVRYzJzWGwzWDA/view

Algorithms Implemented:

1) Generalized Radial Layout Implementation

![alt tag](http://i58.tinypic.com/10zqogk.png)

![alt tag](http://i57.tinypic.com/spfbpe.png)

2) Radail Layout-3

Yee et al's implementation of radial Layout algorithm


![alt tag](http://i60.tinypic.com/2558brt.png)


![alt tag](http://i57.tinypic.com/200qivk.png)


![alt tag](http://i57.tinypic.com/ajnkb6.png)


3) Ringed Circular Layout Tree

Radial view of tree where all children are placed on a circle centered at there parent.

![alt tag](http://i61.tinypic.com/aag0lt.png)

![alt tag](http://i57.tinypic.com/330rc5y.png)

4) Radail Layout via Syemmetric Layout 

Radial view of tree where nodes are placed in hierarchical order centered at the root of tree

![alt tag](http://i61.tinypic.com/nbuj68.png)
