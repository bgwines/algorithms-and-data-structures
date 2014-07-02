{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module LHeap
( empty
, insert
, contains
, find_min
, dequeue_min
, merge
, heap_is_empty
, fromList
, G.graph
) where

{- A heap with the property that the rank of the left child of
   any node is greater than or equal to the rank of the right
   child, where the rank of the node is the minimum of the lengths
   of all its root-null paths. -}

import Test.QuickCheck
import qualified Data.Map as M
import qualified Data.List as L hiding (foldl, foldr)
import Data.Traversable
import Data.Foldable hiding (and, all, elem, foldl, foldr, concatMap)
import Data.Monoid
import Data.Tuple
import Data.Maybe

import Zora.List hiding (merge)
import qualified Zora.TreeGraphing as G
import Zora.Types

import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

data LHeap a = Empty | Node Integer a (LHeap a) (LHeap a)

instance (Show a) => Show (LHeap a) where
	show :: LHeap a -> String
	show Empty = "An empty leftist heap."
	show h = "LeftistHeap " ++ (show $ foldMap (\a -> [a]) h)

instance Functor LHeap where
  fmap f Empty = Empty
  fmap f (Node n e l r) = Node n (f e) (fmap f l) (fmap f r)

instance Foldable LHeap where
	foldMap :: (Monoid m) => (a -> m) -> LHeap a -> m
	foldMap f = zoldMap (f . get_value)

instance (Ord a) => Monoid (LHeap a) where
	mempty :: LHeap a
	mempty = Empty

	mappend :: LHeap a -> LHeap a -> LHeap a
	mappend = merge

instance (Eq a) => Eq (LHeap a) where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(Node _ e l r) == (Node _ e' l' r') = and
		[ e == e'
		, l == l'
		, r == r' ]

instance (Ord a) => Ord (LHeap a) where
	Empty `compare` Empty = EQ
	a     `compare` Empty = GT
	Empty `compare` b     = LT
	(Node _ e _ _) `compare` (Node _ e' _ _) = e `compare` e'

instance Zoldable LHeap where
	zoldMap :: (Monoid m) => (LHeap a -> m) -> LHeap a -> m
	zoldMap = G.zoldMap

instance G.TreeGraphable LHeap where
	value :: LHeap a -> a
	value Empty = error "Empty nodes have no get_value"
	value (Node _ e _ _) = e

	get_children :: LHeap a -> [LHeap a]
	get_children Empty = []
	get_children (Node _ _ l r) = [l, r]

	is_empty :: LHeap a -> Bool
	is_empty Empty = True
	is_empty _ = False

pair :: a -> b -> (a, b)
pair a b = (a, b)

pairAppend :: (a, b) -> c -> (a, b, c)
pairAppend (a, b) c = (a, b, c)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, a') = (f a, f a')

fromList :: (Ord a) => [a] -> LHeap a
fromList = L.foldl' insert empty . L.nub

empty :: LHeap a
empty = Empty

heap_is_empty :: LHeap a -> Bool
heap_is_empty Empty = True
heap_is_empty _ = False

insert :: (Ord a) => LHeap a -> a -> LHeap a
insert lheap e = merge lheap (Node 0 e Empty Empty)

dequeue_min :: (Ord a) => LHeap a -> (a, LHeap a)
dequeue_min Empty = error "Empty heap"
dequeue_min (Node n e l r) = (e, merge l r)

find_min :: LHeap a -> a
find_min Empty = error "Empty heap"
find_min (Node _ e _ _) = e

rank :: LHeap a -> Integer
rank Empty = 0
rank (Node n _ _ _) = n

get_value :: LHeap a -> a
get_value Empty = error "Empty nodes have no get_value"
get_value (Node _ e _ _) = e

merge :: forall a . (Ord a) => LHeap a -> LHeap a -> LHeap a
merge Empty lheap = lheap
merge lheap Empty = lheap
merge
	h1@(Node _ e1 l1 r1)
	h2@(Node _ e2 l2 r2)
	= if e1 < e2
		then make_node e1 l1 (merge r1 h2)
		else make_node e2 l2 (merge h1 r2)
		where
			make_node :: a -> LHeap a -> LHeap a -> LHeap a
			make_node e l r =
				if rank l >= rank r
					then Node ((rank l) + 1) e l r
					else Node ((rank r) + 1) e r l

contains :: (Ord a) => LHeap a -> a -> Bool
contains Empty _ = False
contains h@(Node _ e l r) e' =
	case e' `compare` e of
		LT -> False
		EQ -> True
		GT -> (contains l e') || (contains r e')

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 5000 } test_lheap

test_lheap :: [Integer] -> Bool
test_lheap elems = and
	[ all_elems_present
	, no_other_elems_present
	, has_heap_property lheap
	, has_leftist_property lheap
	, dequeue_min_works lheap
	]
	where
		all_elems_present :: Bool
		all_elems_present = all (LHeap.contains lheap) $ elems

		no_other_elems_present :: Bool
		no_other_elems_present = not . all (LHeap.contains lheap) $ filter (not . (\e -> e `elem` elems)) [-100..100]

		has_heap_property :: (Ord a) => LHeap a -> Bool
		has_heap_property Empty = True
		has_heap_property h@(Node _ e l r) = and
			[ heap_is_empty l || e < get_value l
			, heap_is_empty r || e < get_value r
			, has_heap_property l
			, has_heap_property r ]

		has_leftist_property :: LHeap a -> Bool
		has_leftist_property Empty = True
		has_leftist_property h@(Node _ _ l r) = and
			[ rank l >= rank r
			, has_leftist_property l
			, has_leftist_property r ]

		dequeue_min_works :: (Ord a) => LHeap a -> Bool
		dequeue_min_works h =
			is_sorted
			. (\l -> if l == [] then [] else tail l) 
			. map fst
			. takeWhile (not . heap_is_empty . snd)
			. iterate (dequeue_min . snd)
			$ (undefined, h)

		lheap :: LHeap Integer
		lheap = fromList elems
