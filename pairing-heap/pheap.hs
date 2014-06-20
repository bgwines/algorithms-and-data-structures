{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module PHeap
( empty
, insert
, contains
, find_min
, dequeue_min
, merge
, is_empty
, fromList
, export_for_graphing
) where

{- A heap-ordered multiway tree. It is defined by its behavior during
   the `dequeue_min` operation, which restructures the by merging
   adjacent pairs of children and then folding along that list with
   the merging operation. -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L
import Data.Traversable
import Data.Foldable hiding (and, all, or, elem, concatMap)
import Data.Monoid
import Data.Tuple
import Data.Maybe

import HLib hiding (merge)

import Control.Applicative hiding (empty)

import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

data PHeap a = Empty | Node a [PHeap a]

instance (Show a) => Show (PHeap a) where
	show :: PHeap a -> String
	show Empty = "An empty pairing heap."
	show h = "PairingHeap " ++ (show $ foldMap (\a -> [a]) h)

instance Functor PHeap where
  fmap f Empty        = Empty
  fmap f (Node e children) = Node (f e) (map (fmap f) children)

instance Foldable PHeap where
	foldMap :: (Monoid m) => (a -> m) -> PHeap a -> m
	foldMap f = zoldMap (f . value)

instance Zoldable PHeap where
	zoldMap :: (Monoid m) => (PHeap a -> m) -> PHeap a -> m
	zoldMap f Empty = mempty
	zoldMap f node@(Node e children) =
		(f node) `mappend` (mconcat . map (zoldMap f) $ children)

instance (Ord a) => Monoid (PHeap a) where
	mempty :: PHeap a
	mempty = Empty

	mappend :: PHeap a -> PHeap a -> PHeap a
	mappend = merge

instance (Eq a) => Eq (PHeap a) where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(Node e children) == (Node e' children') = 
		(e == e') &&
		(and $ zipWith (==) children children')

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

export_for_graphing :: forall a. (Show a, Ord a) => PHeap a -> Graph
export_for_graphing Empty = ([], [])
export_for_graphing pheap@(Node e children) = (nodes, edges)
	where
		nodes :: [Node]
		nodes = zip [0..] $ map show' pheap_nodes

		show' :: PHeap a -> Ly.Text
		show' = Ly.pack . show  . value

		pheap_nodes :: [PHeap a]
		pheap_nodes = zoldMap (\a -> [a]) pheap

		edges :: [Edge]
		edges = concatMap edgeify pheap_nodes

		edgeify :: PHeap a -> [Edge]
		edgeify node@(Node e children) =
			map fromJust
			. filter (not . isNothing)
			. map maybe_edge $ children
			where 
				maybe_edge :: PHeap a -> Maybe Edge
				maybe_edge child = if child == Empty
					then Nothing
					else Just
						( m M.! (show' node)
						, m M.! (show' child)
						, Ly.empty )

				m :: M.Map Ly.Text Label
				m = M.fromList $ map swap nodes

fromList :: (Ord a) => [a] -> PHeap a
fromList = L.foldl' insert empty . L.nub

empty :: PHeap a
empty = Empty

is_empty :: PHeap a -> Bool
is_empty Empty = True
is_empty _ = False

merge :: (Ord a) => PHeap a -> PHeap a -> PHeap a
merge a Empty = a
merge Empty b = b
merge a@(Node e children) b@(Node e' children') =
	if e < e'
		then Node e (b : children)
		else Node e' (a : children')

merge_pairs :: (Ord a) => [PHeap a] -> PHeap a
merge_pairs pheaps
	| (length pheaps == 0) = Empty
	| (length pheaps == 1) = head pheaps
	| otherwise =
		let
			h1 = pheaps !! 0
			h2 = pheaps !! 1
			pheaps' = tail . tail $ pheaps
		in
			merge (merge h1 h2) (merge_pairs pheaps')

insert :: (Ord a) => PHeap a -> a -> PHeap a
insert pheap e = merge (Node e []) pheap

dequeue_min :: forall a. (Ord a) => PHeap a -> (a, PHeap a)
dequeue_min Empty = error "Empty heap"
dequeue_min (Node e children) = (e, merge_pairs children)

find_min :: PHeap a -> a
find_min Empty = error "Empty heap"
find_min (Node e _) = e

value :: PHeap a -> a
value Empty = error "Empty nodes have no value"
value (Node e _) = e

get_children :: PHeap a -> [PHeap a]
get_children Empty = error "Empty nodes have no children"
get_children (Node _ children) = children

contains :: (Ord a) => PHeap a -> a -> Bool
contains Empty _ = False
contains h@(Node e children) e' =
	case e' `compare` e of
		LT -> False
		EQ -> True
		GT -> or . map (flip contains $ e') $ children

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 3000 } test_pheap

test_pheap :: [Integer] -> Bool
test_pheap elems = and
	[ all_elems_present
	, no_other_elems_present
	, has_heap_property pheap 
	, dequeue_min_works pheap ]
	where
		all_elems_present :: Bool
		all_elems_present = all (contains pheap) elems

		no_other_elems_present :: Bool
		no_other_elems_present = not . all (contains pheap) $ filter (not . flip elem elems) [-100..100]

		has_heap_property :: (Ord a) => PHeap a -> Bool
		has_heap_property Empty = True
		has_heap_property h@(Node e children) = and
			[ and . map ((<) e . value) $ children
			, and . map has_heap_property $ children ]

		dequeue_min_works :: (Ord a) => PHeap a -> Bool
		dequeue_min_works h =
			is_sorted
			. safe_tail 
			. map fst
			. takeWhile (not . is_empty . snd)
			. iterate (dequeue_min . snd)
			$ (undefined, h)

		pheap :: PHeap Integer
		pheap = fromList elems
