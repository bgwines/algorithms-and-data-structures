
import qualified Data.List as List
import qualified Data.Array as Array

import Control.Applicative
import Test.QuickCheck
import Data.Maybe
import Data.Tuple

length' :: Array.Array Int a -> Int
length' = snd . Array.bounds

find_diff_in_sorted_array :: Array.Array Int Integer -> Integer -> Maybe (Integer, Integer)
find_diff_in_sorted_array array sought_diff'
	| (length' array == 0) = Nothing
	| (sought_diff' == 0) = Just (0, 0)
	| (sought_diff' < 0) = fmap swap result
	| (sought_diff' > 0) = result
	where
		result :: Maybe (Integer, Integer)
		result = find_diff_in_sorted_array' 0 1

		sought_diff :: Integer
		sought_diff = abs sought_diff'

		find_diff_in_sorted_array' :: Int -> Int -> Maybe (Integer, Integer)
		find_diff_in_sorted_array' i j =
			if j == length' array
			 	then Nothing
				else case (l_j - l_i) `compare` sought_diff of
					EQ -> Just (l_i, l_j)
					LT -> find_diff_in_sorted_array' i (succ j)
					GT -> find_diff_in_sorted_array' (succ i) j
				where
					l_i = array Array.! i
					l_j = array Array.! j

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 500 } test

test :: [Integer] -> Bool
test unsorted_list = no_false_negatives && no_false_positives
	where
		list :: [Integer]
		list = List.sort $ unsorted_list

		array :: Array.Array Int Integer
		array = Array.listArray (0, length list) list

		no_false_positives :: Bool
		no_false_positives = (list == []) || all not_false_positive non_present_pair_diffs

		no_false_negatives :: Bool
		no_false_negatives = all not_false_negative pair_diffs

		not_false_positive :: Integer -> Bool
		not_false_positive = isNothing . find_diff_in_sorted_array array

		not_false_negative :: Integer -> Bool
		not_false_negative diff =
			(isJust found_diff) && (is_correct_diff found_diff)
			where
				found_diff :: Maybe (Integer, Integer)
				found_diff = find_diff_in_sorted_array array diff

				is_correct_diff :: Maybe (Integer, Integer) -> Bool
				is_correct_diff (Just (a, b)) = (b - a) == diff

		non_present_pair_diffs :: [Integer]
		non_present_pair_diffs
			= filter (flip notElem $ pair_diffs) [(-m)..m]
			where
				m :: Integer
				m = (maximum list) + (minimum list)

		pair_diffs :: [Integer]
		pair_diffs = List.nub $ (-) <$> list <*> list

main :: IO ()
main = do
	runtests
