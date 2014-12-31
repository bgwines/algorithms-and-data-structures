{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module TTrie
( empty
, insert
, remove
, contains
, match
, elems
, is_empty
, fromList
) where

{- A trie with transitive edges. Noncontiguous matching can be
   very slow in the worst case. 
 -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Array as A
import Data.Monoid
import Data.Tuple
import Data.Maybe

import Zora.List
import qualified Zora.Graphing.DAGGraphing as G

import Control.Applicative hiding (empty)

type Index = Int

type ChildrenMap = M.Map Char TTrie
type TransitiveChildrenMap = M.Map Char [TTrie]
type TransitiveChildrenMapUpdateInfo = [(Char, TTrie, TTrie)]

data TTrie
	= Empty
	| Node (Maybe String) ChildrenMap TransitiveChildrenMap

instance Show TTrie where
	show :: TTrie -> String
	show t = if is_empty t
		then "An empty pairing heap."
		else "TTrie " ++ (show $ elems t)

instance G.DAGGraphable TTrie where
	expand :: TTrie -> Maybe (Maybe String, [(Maybe String, TTrie)])
	expand Empty = Nothing
	expand (Node word cmap trans_cmap) = Just (label, edges)
		where
			label :: Maybe String
			label = fmap (const "•") word

			edges :: [(Maybe String, TTrie)]
			edges = (++)
				(map (show' "") (M.toList cmap))
				(map (show' "*") (concat' . M.toList $ trans_cmap))

			concat' :: [(Char, [TTrie])] -> [(Char, TTrie)]
			concat' = concatMap (\(ch, ts) -> map (\t -> (ch, t)) $ ts)

			show' :: String -> (Char, b) -> (Maybe String, b)
			show' s (a, b) = (Just (a : s), b)

instance Monoid TTrie where
	mempty :: TTrie
	mempty = Empty

	mappend :: TTrie -> TTrie -> TTrie
	mappend a b = fromList $ (elems a) ++ (elems b)

instance Eq TTrie where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(==)
		(Node word  cmap  trans_cmap)
		(Node word' cmap' trans_cmap')
		= and
		[ word == word'
		, cmap == cmap'
		, trans_cmap == trans_cmap' ]

elems :: TTrie -> [String]
elems = flip match $ ""

fromList :: [String] -> TTrie
fromList = L.foldl' insert empty . L.nub

empty :: TTrie
empty = Empty

is_empty :: TTrie -> Bool
is_empty Empty = True
is_empty _ = False

update_trans_cmap :: TransitiveChildrenMapUpdateInfo -> TransitiveChildrenMap -> TransitiveChildrenMap
update_trans_cmap [] trans_cmap = trans_cmap
update_trans_cmap update_info@((ch, old, new) : info') trans_cmap =
	update_trans_cmap info' trans_cmap'
	where
		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = M.insert ch ts trans_cmap
			where
				ts :: [TTrie]
				ts = filter (not . is_empty) $ new : (L.delete old ts') -- won't crash if not `elem`

				ts' :: [TTrie]
				ts' = fromMaybe [] $ M.lookup ch trans_cmap

insert :: TTrie -> String -> TTrie
insert ttrie str = fst $ insert_rec ttrie str str

insert_rec :: TTrie -> String -> String -> (TTrie, TransitiveChildrenMapUpdateInfo)
insert_rec ttrie "" str = (node, [])
	where
		node :: TTrie
		node = case ttrie of
			Empty -> Node (Just str) M.empty M.empty
			(Node _ cmap trans_cmap) -> Node (Just str) cmap trans_cmap

insert_rec Empty (ch:str') str = (node, update_info)
	where
		node :: TTrie
		node = Node Nothing cmap trans_cmap

		cmap :: ChildrenMap
		cmap = M.insert ch only_child M.empty

		trans_cmap :: TransitiveChildrenMap
		trans_cmap = update_trans_cmap update_info_from_below M.empty 

		only_child :: TTrie
		update_info_from_below :: TransitiveChildrenMapUpdateInfo
		(only_child, update_info_from_below) = insert_rec Empty str' str

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level = (ch, Empty, only_child)

insert_rec t@(Node word cmap trans_cmap) (ch:str') str = (node, update_info)
	where
		node :: TTrie
		node = Node word cmap' trans_cmap'

		cmap' :: ChildrenMap
		cmap' = M.insert ch insertion_child_post cmap

		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = update_trans_cmap update_info_from_below trans_cmap

		insertion_child_pre :: TTrie
		insertion_child_pre = fromMaybe Empty $ M.lookup ch cmap

		insertion_child_post :: TTrie
		update_info_from_below :: TransitiveChildrenMapUpdateInfo
		(insertion_child_post, update_info_from_below) = insert_rec insertion_child_pre str' str

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level =
					( ch
					, insertion_child_pre
					, insertion_child_post )

get_word :: TTrie -> Maybe String
get_word Empty = Nothing
get_word (Node word _ _) = word

contains :: TTrie -> String -> Bool
contains Empty _ = False
contains (Node word _ _) "" = not . isNothing $ word
contains t@(Node _ cmap _) (ch:str') =
	if isNothing child
		then False
		else contains (fromJust child) str'
	where
		child :: Maybe TTrie
		child = M.lookup ch cmap

match :: TTrie -> String -> [String]
match ttrie str = uniqueify $ match_rec ttrie str

match_rec :: TTrie -> String -> [String]
match_rec Empty _ = []
match_rec (Node word cmap trans_cmap) "" =
	if isNothing word
		then rec_matches
		else (fromJust word) : rec_matches
		where
			rec_matches :: [String]
			rec_matches
				= catMaybes
				. map get_word
				. L.nub
				. (++) (M.elems cmap)
				$ concat (M.elems trans_cmap)

match_rec t@(Node _ cmap trans_cmap) str@(ch:str') =
	if matching_children == []
		then []
		else concatMap (flip match_rec $ str') matching_children
	where
		matching_children :: [TTrie]
		matching_children = child ++ trans_children
			where
				child :: [TTrie]
				child = maybe [] (\a -> [a]) . M.lookup ch $ cmap

				trans_children :: [TTrie]
				trans_children = fromMaybe [] . M.lookup ch $ trans_cmap

remove :: TTrie -> String -> TTrie
remove t = fst . remove_rec t

remove_rec :: TTrie -> String -> (TTrie, TransitiveChildrenMapUpdateInfo)
remove_rec Empty _ = (Empty, [])
remove_rec (Node _ cmap trans_cmap) "" =
	if M.size cmap == 0
		then (Empty, [])
		else (Node Nothing cmap trans_cmap, [])
remove_rec t@(Node word cmap trans_cmap) str@(ch:str') =
	if isNothing deletion_child_pre
		then (t, [])
		else (t', update_info)
	where
		t' :: TTrie
		t' = if M.size cmap' == 0
			then Empty
			else Node word cmap' trans_cmap'

		deletion_child_pre :: Maybe TTrie
		deletion_child_pre = M.lookup ch cmap

		deletion_child_post :: TTrie
		update_info_from_below :: TransitiveChildrenMapUpdateInfo
		(deletion_child_post, update_info_from_below) = remove_rec (fromJust deletion_child_pre) str'

		cmap' :: ChildrenMap
		cmap' = if is_empty deletion_child_post
			then M.delete ch cmap
			else M.insert ch deletion_child_post . M.delete ch $ cmap

		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = if is_empty deletion_child_post
			then update_trans_cmap update_info' trans_cmap
			else update_trans_cmap update_info_from_below trans_cmap
			where
				update_info' = (ch, fromJust deletion_child_pre, Empty) : update_info_from_below

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level =
					( ch
					, fromJust deletion_child_pre
					, deletion_child_post )

--runtests :: IO ()
--runtests = quickCheckWith stdArgs { maxSuccess = 30 } test_ttrie

--test_ttrie :: [String] -> Bool
--test_ttrie elems = and
--	[ all_elems_present
--	, all all_noncontiguous_substrings_present elems ]
--	where
--		all_elems_present :: Bool
--		all_elems_present = all (contains t) elems

--		all_noncontiguous_substrings_present :: String -> Bool		
--		all_noncontiguous_substrings_present s = all
--			(\subseq -> (match t subseq) /= [])
--			(subsequences s)

--		t :: TTrie
--		t = fromList elems
