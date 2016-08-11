import Test.QuickCheck

import Helper
import Model

test_insertion :: [Int] -> Bool
test_insertion xs = and $ zipWith (>=) result $ drop 1 result
    where result = insertion_sort xs
          insertion_sort [] = []
          insertion_sort [x] = [x]
          insertion_sort (x:xs) = insert x (insertion_sort xs)

main :: IO ()
main = do
    putStrLn "Testing for insertion"
    quickCheck test_insertion
