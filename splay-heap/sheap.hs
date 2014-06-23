{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module SHeap
( export_for_graphing
, fromList
, empty
, is_empty
, elems
, merge
, split
, delete
, join
, (<*)
, find_min
, find_max
, dequeue_max
, dequeue_min
, insert
, contains
) where

{- A heap-ordered self-balancing BST that re-structures itself to allow 
   more frequently accessed elements to be faster to access. -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L hiding (foldl)
import Data.Traversable
import Data.Foldable hiding (and, all, or, elem, foldl, concatMap)
import Data.Monoid
import Data.Tuple
import Data.Maybe

import HLib hiding (merge)

import Control.Applicative hiding (empty, (<*))

import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

data SHeap a = Empty | Node (SHeap a) a (SHeap a)
data SplayCase = L | R | LL | RR | LR | RL | Id

instance (Show a) => Show (SHeap a) where
	show :: SHeap a -> String
	show Empty = "An empty splay heap."
	show h = "SplayHeap " ++ (show $ elems h)

instance Functor SHeap where
  fmap f Empty = Empty
  fmap f (Node l e r) = Node (fmap f l) (f e) (fmap f r)

instance Foldable SHeap where
	foldMap :: (Monoid m) => (a -> m) -> SHeap a -> m
	foldMap f = zoldMap (f . value)

instance Zoldable SHeap where
	zoldMap :: (Monoid m) => (SHeap a -> m) -> SHeap a -> m
	zoldMap f Empty = mempty
	zoldMap f node@(Node l e r) =
		(f node) `mappend` (mconcat . map (zoldMap f) $ [l, r])

instance (Ord a) => Monoid (SHeap a) where
	mempty :: SHeap a
	mempty = Empty

	mappend :: SHeap a -> SHeap a -> SHeap a
	mappend = merge 

instance (Eq a) => Eq (SHeap a) where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(Node l e r) == (Node l' e' r') = and
		[ e == e'
		, l == l'
		, r == r' ]

instance (Ord a) => Ord (SHeap a) where
	Empty `compare` Empty = EQ
	_     `compare` Empty = GT
	Empty `compare` _     = LT
	(Node _ e _) `compare` (Node _ e' _) = e `compare` e'

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

export_for_graphing :: forall a. (Show a, Ord a) => SHeap a -> Graph
export_for_graphing Empty = ([], [])
export_for_graphing sheap@(Node l e r) = (nodes, edges)
	where
		nodes :: [Node]
		nodes = zip [0..] $ map show' sheap_nodes

		show' :: SHeap a -> Ly.Text
		show' = Ly.pack . show  . value

		sheap_nodes :: [SHeap a]
		sheap_nodes = zoldMap (\a -> [a]) sheap

		edges :: [Edge]
		edges = concatMap edgeify sheap_nodes

		edgeify :: SHeap a -> [Edge]
		edgeify node@(Node l e r) =
			map fromJust
			. filter (not . isNothing)
			. map maybe_edge $ [l, r]
			where 
				maybe_edge :: SHeap a -> Maybe Edge
				maybe_edge child = if child == Empty
					then Nothing
					else Just
						( m M.! (show' node)
						, m M.! (show' child)
						, Ly.empty )

				m :: M.Map Ly.Text Label
				m = M.fromList $ map swap nodes

fromList :: (Ord a) => [a] -> SHeap a
fromList = L.foldl' insert empty

empty :: SHeap a
empty = Empty

is_empty :: SHeap a -> Bool
is_empty Empty = True
is_empty _ = False

elems :: SHeap a -> [a]
elems = foldMap (\e -> [e])

merge :: (Ord a) => SHeap a -> SHeap a -> SHeap a
merge a Empty = a
merge Empty b = b
merge a b = fromList $ (elems a) ++ (elems b)

split :: (Ord a) => SHeap a -> a -> (SHeap a, SHeap a)
split h x = (l, r)
	where
		_'@(Node l x' r) = splay h x

delete :: (Ord a) => SHeap a -> a -> SHeap a
delete h = (\(l, r) -> join l r) . split h

join :: (Ord a) => SHeap a -> SHeap a -> SHeap a
join a b = if a <* b
	then join' a b
	else if b <* a
		then join' b a
		else merge a b

(<*) :: (Ord a) => SHeap a -> SHeap a -> Bool
a <* b = and $ (<) <$> (elems a) <*> (elems b)
-- TODO: compare min to max instead

-- a <* b
join' :: (Ord a) => SHeap a -> SHeap a -> SHeap a
join' a b = Node a' x b
	where
		(a', x) = dequeue_max a

find_min :: (Ord a) => SHeap a -> (SHeap a, a)
find_min h = (insert h' min_value, min_value)
	where
		(h', min_value) = dequeue_min h

find_max :: (Ord a) => SHeap a -> (SHeap a, a)
find_max h = (insert h' max_value, max_value)
	where
		(h', max_value) = dequeue_max h

dequeue_min :: (Ord a) => SHeap a -> (SHeap a, a)
dequeue_min Empty = error "Empty heap"
dequeue_min (Node Empty e r) = (r, e)
dequeue_min (Node l e r) = (Node l' e r, min_value)
	where
		(l', min_value) = dequeue_min l

dequeue_max :: (Ord a) => SHeap a -> (SHeap a, a)
dequeue_max Empty = error "Empty heap"
dequeue_max (Node l e Empty) = (l, e)
dequeue_max (Node l e r) = (Node l e r', max_value)
	where
		(r', max_value) = dequeue_max r

insert :: forall a. (Ord a) => SHeap a -> a -> SHeap a
insert Empty e = Node Empty e Empty
insert sheap e = splay sheap' e
	where
		sheap' :: SHeap a
		sheap' = if contains_without_splay sheap e
			then sheap
			else insert_without_splay sheap e

insert_without_splay :: (Ord a) => SHeap a -> a -> SHeap a
insert_without_splay Empty x = Node Empty x Empty
insert_without_splay h@(Node l e r) x =
	case x `compare` e of
		EQ -> h
		LT -> Node (insert l x) e r
		GT -> Node l e (insert r x)

---------------------------
---------------------------

identify_case :: (Ord a) => SHeap a -> a -> SplayCase
identify_case Empty _ = error "Empty root"
identify_case h@(Node l e r) x =
	case x `compare` e of
		EQ -> Id
		LT -> case l of
			Empty -> error "Nothing there."
			Node _ el _ -> case x `compare` el of
				EQ -> L
				LT -> LL
				GT -> LR
		GT -> case r of
			Empty -> error "Nothing there."
			Node _ er _ -> case x `compare` er of
				EQ -> R
				LT -> RL
				GT -> RR

splay :: (Ord a) => SHeap a -> a -> SHeap a
splay Empty x = error "Element not in tree"
splay h@(Node l e r) x =
	case identify_case h x of
		L  -> splay_l  h x
		R  -> splay_r  h x
		LL -> splay_ll h x
		RR -> splay_rr h x
		LR -> splay_lr h x
		RL -> splay_rl h x
		Id -> h

splay_l :: (Ord a) => SHeap a -> a -> SHeap a
splay_l h@(Node l e r) e' = Node ll x (Node lr e r)
	where
		_'@(Node ll x lr) = l

splay_r :: (Ord a) => SHeap a -> a -> SHeap a
splay_r h@(Node l e r) e' = Node (Node l e rl) x rr
	where
		_'@(Node rl x rr) = r

-- sub-trees numbered h1-4 in L->R order
splay_ll :: (Ord a) => SHeap a -> a -> SHeap a
splay_ll h@(Node l e3 h4) x
	= Node h1 e1 (Node h2 e2 (Node h3 e3 h4))
	where
		_'@(Node ll e2 h3) = l
		_''@(Node h1 e1 h2) = splay ll x

-- sub-heaps numbered h1-4 in L->R order
splay_rr :: (Ord a) => SHeap a -> a -> SHeap a
splay_rr h@(Node h1 e1 r) x
	= Node (Node (Node h1 e1 h2) e2 h3) e3 h4
	where
		_'@(Node h2 e2 rr) = r
		_''@(Node h3 e3 h4) = splay rr x

-- sub-trees numbered h1-4 in L->R order
splay_lr :: (Ord a) => SHeap a -> a -> SHeap a
splay_lr h@(Node l e3 h4) x
	= Node (Node h1 e1 h2) e2 (Node h3 e3 h4)
	where
		_'@(Node h1 e1 lr) = l
		_''@(Node h2 e2 h3) = splay lr x

-- sub-trees numbered h1-4 in L->R order
splay_rl :: (Ord a) => SHeap a -> a -> SHeap a
splay_rl h@(Node h1 e1 r) x
	= Node (Node h1 e1 h2) e2 (Node h3 e3 h4)
	where
		_'@(Node rl e3 h4) = r
		_''@(Node h2 e2 h3) = splay rl x

---------------------------
---------------------------

value :: SHeap a -> a
value Empty = error "Empty nodes have no value"
value (Node l e r) = e

get_children :: SHeap a -> [SHeap a]
get_children Empty = error "Empty nodes have no children"
get_children (Node l _ r) = [l, r]

contains :: (Ord a) => SHeap a -> a -> (Bool, SHeap a)
contains h x = if contains_without_splay h x
	then (True, splay h x)
	else (False, h)

contains_without_splay :: (Ord a) => SHeap a -> a -> Bool
contains_without_splay Empty _ = False
contains_without_splay h@(Node l e r) x =
	case x `compare` e of
		EQ -> True
		LT -> contains_without_splay l x
		GT -> contains_without_splay r x

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 3000 } test_sheap

test_sheap :: [Integer] -> Bool
test_sheap elements = and
	[ all_elems_present
	, no_other_elems_present
	, has_bst_property sheap 
	, dequeue_min_works sheap ]
	where
		all_elems_present :: Bool
		all_elems_present = all (fst . contains sheap) elements

		no_other_elems_present :: Bool
		no_other_elems_present = not . all (fst . contains sheap) $ other_elems

		has_bst_property :: (Ord a) => SHeap a -> Bool
		has_bst_property Empty = True
		has_bst_property h@(Node l e r) = and
			[ all ((>) e) . elems $ l
			, all ((<) e) . elems $ r
			, all has_bst_property $ [l, r] ]

		dequeue_min_works :: (Ord a) => SHeap a -> Bool
		dequeue_min_works h =
			is_sorted
			. safe_tail 
			. map fst
			. takeWhile (not . is_empty . fst)
			. iterate (dequeue_min . fst)
			$ (h, undefined)

		sheap :: SHeap Integer
		sheap = fromList elements

		other_elems :: [Integer]
		other_elems = filter (not . flip elem elements) [-100..100]
