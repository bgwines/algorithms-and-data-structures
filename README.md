algorithms-and-data-structures
==========================

Data structure and implementations. Those in in functional languages mostly follow *Purely Functional Data Structures*, by Chris Okasaki.

Dependencies
------------

- [Haskell](http://www.haskell.org/haskellwiki/Haskell)
- [GraphViz](http://graphviz.org/) (`brew install graphviz`)
- [Haskell GraphViz](https://hackage.haskell.org/package/graphviz) (`cabal install graphviz`)
- [Zora](https://hackage.haskell.org/package/Zora) (`cabal install zora`)

Skew binary random access lists
-------------------------------
A random-access list is a data structure that supports most list operations (e.g. `cons`, `head`, `tail`), but also indexing operations such as `set` and `at`.

This implemented usines a sparse representation of skew binary numbers. It is a numerical representation backed by a list of complete binary trees.

The skew binary number system is a positional number system where the weights are not `2^i`, as in traditional binary, but rather `2^(i+1) - 1`. Digits are allowed to be `0`, `1`, or `2`, with the restriction that only the least significant nonzero digit is allowed to be `2`. With this restriction, every natural number has a unique representation under this system [Mye83]; otherwise, the system is oredundant.

We represent the digit `2` at position `i` in our sparse representation as the concatenation of two trees of order `w_i`.

List operations (`cons`, `head`, and `tail`) run in constant time, whereas array operations (`set` and `at`) run in logarithmic time (more specifically, `min(i, log n)`, where `i` is the index in question).

Dynamic range trees
-------------------
A dynamic range tree is a data structure that supports two main operations: querying the difference of the maximum and minimum values in the a contiguous range, and incrementing all values in a contiguous range. This tree supports those operations, but operates not on an array, but a B-tree. Updates and queries are performed not on ranges, but on subtrees.

Disjoint-set forests
--------------------
A disjoint-set forest is a partitioning of a set that 
supports `union` (merge two sets) and `find` (find the 
representative of the set in which an element is) in log* 
time. 

The data structure is attributed to McIlroy and Morris in
the 70s, but its complexity was first analyzed by Tarjan,
many years later.

Leftist heaps
------------
A leftist heap is a (binary) heap with the property that the rank of the left child of any node is greater than or equal to the rank of the right child, where the rank of the node is the minimum of the lengths of all its root-null paths.

Pairing heaps
-------------
A pairing heap is a heap-ordered multiway tree. It is defined by its behavior during the `dequeue_min` operation, which restructures the by merging adjacent pairs of children and then folding along that list with the merging operation.

Splay heaps
-----------
A splay heap is a heap-ordered self-balancing BST that re-structures itself through smarter-than-naïve tree rotations to allow more frequently accessed elements to be faster to access.

Tries
-----
A trie is a tree that stores strings in a manner that overlaps all common prefixes.

Transitive tries
----------------
A trie with transitive edges. Makes noncontiguous matching very straightforward, but is highly dependent on the amount of overlap between strings in the corpus.

Cartesian trees
---------------
A cartesian tree is a heap-ordered tree that reperesents a list where in-order traversal produces the original list.

Fused cartesian trees
---------------------
A fused cartesian tree is a cartesian tree where if a node has the same value as its parent, the two are fused together.

array-pair-diff
---------------
In linear time, identify whether an array contains two elements whose difference is a specified number.

The look-and-say sequence
-------------------------
	1
	11 (prev is "one 1")
	21 (prev is "two 1s")
	1211 (prev is "one 2; one 1")
	111221 (prev is "one 1; one 2; two 1s")
	312211 (prev is "three 1s; two 2s; one 1")
	etc.

