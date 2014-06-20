functional-data-structures
==========================

Data structure implementations in functional languages (predominantly Haskell), following *Purely Functional Data Structures*, by Chris Okasaki.

Dependencies
------------

- [Haskell](http://www.haskell.org/haskellwiki/Haskell)
- [GraphViz](http://graphviz.org/) (`brew install graphviz`)
- [Haskell GraphViz](https://hackage.haskell.org/package/graphviz) (`cabal install graphviz`)
- [HLib](https://github.com/bgwines/hlib)

Leftist heaps
------------
A (binary) heap with the property that the rank of the left child of any node is greater than or equal to the rank of the right child, where the rank of the node is the minimum of the lengths of all its root-null paths.