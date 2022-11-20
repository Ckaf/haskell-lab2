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
test4 = TestCase (assertEqual "Test sort" expected_list (append tr actual_list))
                  where
                    expected_list = [7, 6, 4, 2, 1]
                    actual_list = []
                    tr = add_elements emptyTree $ reverse expected_list
                    append xs ys = foldr (\x y -> x:y) ys xs

                    

tests :: Test
tests = TestList [ test1, test2, test3 ]
