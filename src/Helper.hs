module Helper 
    ( insert 
    , splitBS
    , toPort ) where

import Prelude hiding           ( drop
                                , take )
import Data.ByteString.Char8    ( unpack)

import Data.ByteString          ( drop
                                , take
                                , ByteString )

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x >= y    = x:y:ys
    | otherwise = y:(insert x ys)

-- converts a list of Integers in Hex form to a single Int
toPort :: [Integer] -> Integer
toPort [] = 0
toPort (x:[]) = x
toPort (x:xs) = (x * 256) + toPort xs

-- splits a ByteString into the list of Ints given
splitBS :: ByteString -> [Int] -> [ByteString]
splitBS _ [] = []
splitBS bs (x:xs) = take x bs : splitBS (drop x bs) xs
