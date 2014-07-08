{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Trie
( empty
, insert
, remove
, contains
, is_empty
, fromList
, G.graph
) where

{- A heap with the property that the rank of the left child of
   any node is greater than or equal to the rank of the right
   child, where the rank of the node is the minimum of the 
   lengths of all its root-null paths. -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L
import Data.Monoid
import Data.Tuple
import Data.Maybe

import Zora.List hiding (merge)
import qualified Zora.Graphing.DAGGraphing as G

import Control.Applicative hiding (empty)

type ChildrenMap = M.Map Char Trie

data Trie = Empty | Node (Maybe String) ChildrenMap

instance Show Trie where
	show :: Trie -> String
	show t = if is_empty t
		then "An empty pairing heap."
		else "Trie " ++ (show $ elems t)

instance G.DAGGraphable Trie where
	expand :: Trie -> Maybe (Maybe String, [(Maybe String, Trie)])
	expand Empty = Nothing
	expand (Node word cmap) = Just (label, edges)
		where
			label :: Maybe String
			label = fmap (const "•") word

			edges :: [(Maybe String, Trie)]
			edges = map show' (M.toList cmap)

			show' :: (Char, b) -> (Maybe String, b)
			show' (a, b) = (Just (a : ""), b)

instance Monoid Trie where
	mempty :: Trie
	mempty = Empty

	mappend :: Trie -> Trie -> Trie
	mappend a b = fromList $ (elems a) ++ (elems b)

instance Eq Trie where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(==)
		(Node word  cmap)
		(Node word' cmap')
		= and
		[ word == word'
		, cmap == cmap' ]

elems :: Trie -> [String]
elems Empty = []
elems (Node word cmap) =
	if isNothing word
		then rec_words
		else (fromJust word) : rec_words
		where
			rec_words :: [String]
			rec_words = concatMap elems . M.elems $ cmap

fromList :: [String] -> Trie
fromList = L.foldl' insert empty . L.nub

empty :: Trie
empty = Empty

is_empty :: Trie -> Bool
is_empty Empty = True
is_empty _ = False

insert :: Trie -> String -> Trie
insert trie str = insert_rec str str trie

insert_rec :: String -> String -> Trie -> Trie
insert_rec "" str Empty = Node (Just str) M.empty
insert_rec "" str (Node _ cmap) = Node (Just str) cmap
insert_rec (ch:str') str Empty = Node Nothing cmap
	where
		cmap :: ChildrenMap
		cmap = M.insert ch child M.empty

		child :: Trie
		child = insert_rec str' str Empty

insert_rec (ch:str') str (Node word cmap) = Node word cmap'
	where
		cmap' :: ChildrenMap
		cmap' = M.insert ch insertion_child_post cmap

		insertion_child_pre :: Trie
		insertion_child_pre = fromMaybe Empty $ M.lookup ch cmap

		insertion_child_post :: Trie
		insertion_child_post = insert_rec str' str insertion_child_pre

contains :: Trie -> String -> Bool
contains Empty _ = False
contains (Node word _) "" = not . isNothing $ word
contains t@(Node _ cmap) (ch:str') =
	if isNothing $ M.lookup ch cmap
		then False
		else contains (cmap M.! ch) str'

remove :: Trie -> String -> Trie
remove Empty _ = Empty
remove (Node _ cmap) "" =
	if M.size cmap == 0
		then Empty
		else (Node Nothing cmap)
remove t@(Node word cmap) str@(ch:str') =
	if isNothing deletion_child_pre
		then t
		else t'
	where
		t' :: Trie
		t' = if M.size cmap' == 0
			then Empty
			else Node word cmap'

		deletion_child_pre :: Maybe Trie
		deletion_child_pre = M.lookup ch cmap

		deletion_child_post :: Trie
		deletion_child_post = remove (fromJust deletion_child_pre) str'

		cmap' :: ChildrenMap
		cmap' = if is_empty deletion_child_post
			then M.delete ch cmap
			else M.insert ch deletion_child_post . M.delete ch $ cmap
