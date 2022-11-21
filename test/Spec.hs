import Test.HUnit
import RedBlackTree

main :: IO ()
main = runTestTTAndExit tests


add_elements:: (Ord a) => Tree a -> [a] -> Tree a
add_elements t list = foldl (\x y ->insert y x) t list


check_elements:: (Ord a) => Tree a -> [a] -> [Bool]
check_elements t list = map is_exist list where
  is_exist x = is_member x t

test1 :: Test
test1 = TestCase (assertEqual "Test with int" (replicate 10 True) (check_elements one_to_ten [1..10]))
                  where one_to_ten = add_elements emptyTree [1..10]


test2 :: Test
test2 = TestCase (assertEqual "Test with string" (replicate 6 True) (check_elements string_to_insert strings))
                  where string_to_insert = add_elements emptyTree strings
                        strings = ["1", "1", "2", "3", "hello", "world"]


test3 :: Test
test3 = TestCase (assertEqual "Test foldr +" ( foldr (+) 5 [1..10] ) (foldr (+) 5 tr))
                  where
                    tr = add_elements emptyTree [1..10]

test4 :: Test
test4 = TestCase (assertEqual "Test sort from less to more" expected_list (append tr actual_list))
                  where
                    expected_list = [1, 2, 3, 4, 5, 6]
                    actual_list = []
                    tr = add_elements emptyTree $ reverse expected_list
                    append xs ys = foldr (\x y -> x:y) ys xs

test5 :: Test
test5 = TestCase (assertEqual "Test sort from more to less" expected_list (append tr actual_list))
                  where
                    expected_list = [6, 5, 4, 3, 2, 1]
                    actual_list = []
                    tr = add_elements emptyTree [3, 4, 5, 1, 2, 6]
                    append xs ys = foldl (\x y -> y:x) ys xs

test6 ::Test
test6 = TestCase (assertEqual "Test manoid" expected_list (append (tree1<>tree2) actual_list))
                  where
                    expected_list = [6, 5, 4, 3, 2, 1]
                    actual_list = []
                    tree1 = add_elements emptyTree [1, 2, 3]
                    tree2 = add_elements emptyTree [6, 5, 4]
                    append xs ys = foldl (\x y -> y:x) ys xs

test7 ::Test
test7 = TestCase (assertEqual "Test manoid (empty case)" expected_list (append (tree1<>tree2) actual_list))
                  where
                    expected_list = [6, 5, 4, 3, 2, 1]
                    actual_list = []
                    tree1 = add_elements emptyTree expected_list
                    tree2 = add_elements emptyTree []
                    append xs ys = foldl (\x y -> y:x) ys xs

test8 ::Test
test8 = TestCase (assertEqual "Test fmap" expected_list (append (fmap (+1) tr) actual_list))
                  where
                    expected_list = [6, 5, 4, 3, 2, 1]
                    actual_list = []
                    tr = add_elements emptyTree [5, 4, 3, 2, 1, 0]
                    append xs ys = foldl (\x y -> y:x) ys xs

test9 ::Test
test9 = TestCase (assertEqual "Test filter" expected_list (filterT (>4) tr ))
                  where
                    expected_list = [5, 6]
                    tr = add_elements emptyTree [6, 5, 4, 3, 2, 1]

tests :: Test
tests = TestList [ test1, test2, test3, test4, test5, test6, test7, test8, test9 ]
