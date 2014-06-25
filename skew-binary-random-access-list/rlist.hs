{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RList
( RList(..)
, RList.cons
, RList.head
, RList.tail
, fromList
, (!)
, at
, set
, empty
, is_empty
, aslist
, size
, export_for_graphing
) where

{- A random-access list is a data structure that supports most list
   operations (e.g. `cons`, `head`, `tail`), but also indexing
   operations such as `set` and `at`.

   This implemented usines a sparse representation of skew binary
   numbers. It is a numerical representation backed by a list of
   complete binary trees.

   The skew binary number system is a positional number system where
   the weights are not 2^i, as in traditional binary, but rather
   2^(i+1) - 1. Digits are allowed to be 0, 1, or 2, with the
   restriction that only the least significant nonzero digit is allowed
   to be 2. With this restriction, every natural number has a unique
   representation under this system [Mye83]; otherwise, the system is 
   redundant.

   We represent the digit 2 at position `i` in our sparse representation 
   as the concatenation of two trees of order `w_i`.

   List operations (`cons`, `head`, and `tail`) run in constant time,
   whereas array operations (`set` and `at`) run in logarithmic time
   (more specifically, `min(i, log n)`, where `i` is the index in 
   question). -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L hiding (foldl, sum)
import Data.Traversable
import Data.Foldable hiding (and, all, or, elem, foldl, concatMap, sum)
import Data.Monoid
import Data.Tuple
import Data.Maybe

import HLib hiding (cons)

import Control.Applicative hiding (empty)

import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

data Tree a
	= Leaf a
	| Node a (Tree a) (Tree a)
	| Meganode a [Tree a] deriving Show

data RList a = RList [(Integer, Tree a)] deriving Show

{-instance (Show a) => Show (RList a) where
	show :: RList a -> String
	show Empty = "An empty pairing heap."
	show h = "Heap " ++ (show $ elems h)
-}

-- instance Functor (Integer, Tree a) where
-- 	fmap f (n, t) = (n, fmap f t)

instance Functor Tree where
	fmap f (Leaf e) = Leaf (f e)
	fmap f (Node e l r) = Node (f e) (fmap f l) (fmap f r)

instance Foldable Tree where
	foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
	foldMap f = zoldMap (f . value)

instance Zoldable Tree where
	zoldMap :: (Monoid m) => (Tree a -> m) -> Tree a -> m
	zoldMap f node@(Leaf a) = f node
	zoldMap f node@(Node e l r)
		= (f node) `mappend` (mconcat . map (zoldMap f) $ [l, r])
	zoldMap f node@(Meganode e children)
		= (f node) `mappend` (mconcat . map (zoldMap f) $ children)
instance (Eq a) => Eq (Tree a) where
	(Leaf e)     == (Leaf e')    = (e == e')
	(Node _ _ _) == (Leaf _)     = False
	(Leaf _)     == (Node _ _ _) = False
	(Node e l r) == (Node e' l' r') = and
		[ e == e'
		, l == l'
		, r == r' ]

instance Functor RList where
	fmap :: (a -> b) -> RList a -> RList b
	fmap f = RList . map f' . list
		where
			f' (n, t) = (n, fmap f t)

instance Foldable RList where
	foldMap :: (Monoid m) => (a -> m) -> RList a -> m
	foldMap f = mconcat . map (foldMap f . snd) . list

-- this should compile, in theory...
--instance Zoldable RList where
--	zoldMap :: (Monoid m) => (RList a -> m) -> RList a -> m
--	zoldMap f = mconcat . map (zoldMap f . snd) . list

instance (Eq a) => Eq (RList a) where
	a == b = ((list a) == (list b))

uid :: (Show a) => Tree a -> String
uid = foldMap show

value :: Tree a -> a
value (Leaf e) = e
value (Node e _ _) = e
value (Meganode e _) = e

children :: Tree a -> [Tree a]
children (Leaf _) = []
children (Node _ l r) = [l, r]
children (Meganode _ nodes) = nodes

tree_size :: (Integer, Tree a) -> Integer
tree_size = fst

list :: RList a -> [(Integer, Tree a)]
list (RList l) = l

{- `cons` is analogous to the successor function, since we are using
   a numerical representation.

   With skew binary numbers, if the least significant nonzero digit
   of a number `n` is a 2, then `S(n)` can be calculated by
   incrementing the next most significant digit and setting the 2
   to 0. Otherwise, we increment the least significant digit (note
   that this implementation for both 0->1 and 1->2). The final case, if
   it is already 2, cannot occur, since that would mean we would have
   falled under the first case. -}
cons :: a -> RList a -> RList a
cons e (RList ((w,t):(w',t'):ts)) =
	if w == w'
		then RList ((1 + w + w', (Node e t t')) : ts)
		else RList ((1, Leaf e) : (w, t) : (w', t') : ts)
cons e (RList ts) = RList ((1, Leaf e) : ts)

head :: RList a -> a
head (RList ((1,(Leaf e)):ts)) = e
head (RList ((_,(Node e l r)):ts)) = e

tail :: RList a -> RList a
tail (RList ((1,(Leaf e)):ts)) = RList ts
tail (RList ((w,(Node e l r)):ts)) = RList
		( (w `div` 2, l)
		: (w `div` 2, r)
		: ts )

(!) :: RList a -> Integer -> a
(RList ((w,t):ts)) ! i =
	if i < w
		then lookup_tree w t i
		else (RList ts) ! (i - w)
(RList []) ! i = error "Empty list"

lookup_tree :: Integer -> Tree a -> Integer -> a
lookup_tree 1 (Leaf e) 0 = e
lookup_tree 1 (Leaf e) _ = error "Uh-oh."
lookup_tree _ (Node e _ _) 0 = e
lookup_tree w (Node e l r) i =
	if i <= (w `div` 2)
		then lookup_tree w' l (i - 1)
		else lookup_tree w' r (i - 1 - w')
		where w' = w `div` 2

at = (!)

set :: RList a -> Integer -> a -> RList a
set (RList ((w,t):ts)) i x =
	if i < w
		then RList ((w, set_tree w t i x) : ts)
		else RList ((w,t) : (list $ set (RList ts) (i - w) x))

set_tree :: Integer -> Tree a -> Integer -> a -> Tree a
set_tree 1 (Leaf e) 0 x = Leaf x
set_tree _ (Node e l r) 0 x = Node x l r
set_tree w (Node e l r) i x =
	if i < w
		then Node e (set_tree w' l (i - 1) x) r
		else Node e l (set_tree w' r (i - 1 - w') x)
		where w' = w `div` 2

empty :: RList a
empty = RList []

is_empty :: RList a -> Bool
is_empty (RList []) = True
is_empty (RList _) = False

fromList :: [a] -> RList a
fromList = L.foldl' (flip cons) empty . reverse

aslist :: RList a -> [a]
aslist rlist =
	map ((!) rlist) [0..((size rlist) - 1)]

size :: RList a -> Integer
size = sum . map fst . list

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

export_for_graphing :: forall a. (Show a, Ord a) => RList a -> Graph
export_for_graphing (RList []) = ([], [])
export_for_graphing (RList rlist) = (nodes, edges)
	where
		grand_tree :: Tree a
		grand_tree = Meganode e_dummy (map snd rlist)
			where
				e_dummy = RList.head (RList rlist)

		all_tree_nodes :: [Tree a]
		all_tree_nodes = zoldMap (\a -> [a]) grand_tree

		all_tree_nodes_with_tree_ids :: [(Tree a, Integer)]
		all_tree_nodes_with_tree_ids = zip all_tree_nodes tree_ids
			where
				tree_ids :: [Integer]
				tree_ids = 0 : (concatMap (\(n, size) -> replicate (fromIntegral size) n) tree_ids')

				tree_ids' :: [(Integer, Integer)]
				tree_ids' = zip [1..] (map fst rlist)

		nodes :: [Node]
		nodes = filter used . zip [0..] . map show' $ all_tree_nodes
			where
				used :: Node -> Bool
				used node = or . map (edge_uses node) $ edges

				edge_uses :: Node -> Edge -> Bool
				edge_uses (node, _) (u, v, _)
					= (u == node) || (v == node) 

		show' :: Tree a -> Ly.Text
		show' (Meganode _ _) = Ly.empty
		show' node = Ly.pack . show . value $ node

		edges :: [Edge]
		edges = concatMap edgeify all_tree_nodes_with_tree_ids

		edgeify :: (Tree a, Integer) -> [Edge]
		edgeify (node, tree_id) =
			if tree_id == 0
				then map edgify_node' . zip [1..] . children $ node
				else map (edgify_node tree_id) . children $ node  -- children will have the same tree_id because they're in the same tree
			where 
				edgify_node' :: (Integer, Tree a) -> Edge
				edgify_node' (child_tree_id, child) = edgify_node child_tree_id child

				edgify_node :: Integer -> Tree a -> Edge
				edgify_node child_tree_id child =
					( m M.! (show_unique (node, tree_id))
					, m M.! (show_unique (child, child_tree_id))
					, Ly.empty )

				m :: M.Map Ly.Text Label
				m = M.fromList $ zip (map show_unique all_tree_nodes_with_tree_ids) [0..]

				show_unique :: (Tree a, Integer) -> Ly.Text
				show_unique (node, tree_id)
					= Ly.pack ((uid node) ++ (show tree_id))

