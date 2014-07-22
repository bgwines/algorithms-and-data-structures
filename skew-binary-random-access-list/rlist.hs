{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RList
( RList(..)
, RList.cons
, RList.head
, RList.tail
, RList.graph
, fromList
, (!)
, at
, set
, empty
, is_empty
, aslist
, size
, elems
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

import Zora.List hiding (cons)
import Zora.Graphing.DAGGraphing as G

import Control.Applicative hiding (empty)

import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

data Tree a
	= Leaf a
	| Node a (Tree a) (Tree a)
	| Meganode a [Tree a] deriving Show

data RList a = RList [(Integer, Tree a)]

instance (Show a) => Show (RList a) where
	show :: RList a -> String
	show rlist = "RList " ++ (show $ elems rlist)

instance Functor Tree where
	fmap f (Leaf e) = Leaf (f e)
	fmap f (Node e l r) = Node (f e) (fmap f l) (fmap f r)

instance Foldable Tree where
	foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
	foldMap f (Node e l r) = (f e) `mappend` (mconcat . map (foldMap f) $ [l, r])

instance (Eq a) => Eq (Tree a) where
	(Leaf e)     == (Leaf e')    = (e == e')
	(Node e l r) == (Node e' l' r') = and
		[ e == e'
		, l == l'
		, r == r' ]
	(Meganode e nodes) == (Meganode e' nodes') = and
		[ e == e'
		, nodes == nodes' ]

	(Leaf _)       == _ = False
	(Node _ _ _)   == _ = False
	(Meganode _ _) == _ = False

instance Functor RList where
	fmap :: (a -> b) -> RList a -> RList b
	fmap f = RList . map f' . list
		where
			f' (n, t) = (n, fmap f t)

instance Foldable RList where
	foldMap :: (Monoid m) => (a -> m) -> RList a -> m
	foldMap f = mconcat . map (foldMap f . snd) . list

instance (Eq a) => Eq (RList a) where
	a == b = ((list a) == (list b))

uid :: (Show a) => Tree a -> String
uid = foldMap show

instance (Show a) => G.DAGGraphable (Tree a) where
	expand :: Tree a -> Maybe (Maybe String, [(Maybe String, Tree a)])
	expand (Leaf e) = Just (Just (show e), [])
	expand (Node e l r) = Just (Just (show e), [(Nothing, l), (Nothing, r)])
	expand (Meganode _ nodes) = Just (Nothing, map (\x -> (Nothing, x)) nodes)

elems :: RList a -> [a]
elems (RList list) = concatMap (elems_tree . snd) list

elems_tree :: Tree a -> [a]
elems_tree (Leaf e) = [e]
elems_tree (Node e l r) = e : ((elems_tree l) ++ (elems_tree r))

graph :: forall a. (Show a, Eq a) => RList a -> IO String
graph rlist = G.graph grand_tree
	where
		grand_tree :: Tree a
		grand_tree = Meganode (RList.head rlist) (map snd . list $ rlist)

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
