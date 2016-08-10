module Main where

import qualified Data.ByteString.Char8 as C8    ( unpack)
import qualified Data.ByteString as B           ( drop
                                                , splitAt
                                                , take
                                                , unpack
                                                , ByteString(..) )

import Control.Monad.IO.Class                   ( liftIO )
import Data.Conduit                             ( (=$)
                                                , (=$=)
                                                , ($$)
                                                , await
                                                , yield
                                                , Conduit 
                                                , Sink)
import Data.Conduit.List                        ( mapMaybe )
import Network.Pcap.Conduit                     ( sourceOffline
                                                , Packet )
import System.Environment                       ( getArgs )

import Lib
import Model

main :: IO ()
main = do
    args <- getArgs
    if "r" `elem` args
       then sourceOffline("mdf-kospi200.20110216-0.pcap") $$ (toPacket =$= reorderPacket)=$ printPacket
       else sourceOffline("mdf-kospi200.20110216-0.pcap") $$ toPacket =$ printPacket

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x >= y    = x:y:ys
    | otherwise = y:(insert x ys)

toPort :: [Integer] -> Integer
toPort (x:[]) = x
toPort (x:xs) = (x * 256) + toPort xs

reorderPacket :: Conduit QuotePacket IO QuotePacket
reorderPacket = loop []
    where yieldItem x = do
              if length x == 1
                  then yield $ last x
                  else do
                      yield $ last x
                      yieldItem $ init x
          loop xs = do
              x <- await
              case x of
                  Nothing -> yieldItem xs
                  Just x -> do
                      let sorted = insert x xs 
                      if getQuoteAcceptTime (last sorted) + 300 < getQuoteAcceptTime x
                          then do
                              yield $ last sorted
                              loop $ init sorted
                          else loop sorted


toPacket :: Conduit Packet IO QuotePacket
toPacket = mapMaybe toQuotePacket where
    toQuotePacket (hdr, bs)
        | (C8.unpack . B.take 5) content /= "B6034"             = Nothing
        | not $ destPort `elem` [15515, 15516]                  = Nothing
        | otherwise                                             = Just $ QuotePacket marketType issueSeqNo quoteAcceptTime
        where (header, content) = B.splitAt 42 bs
              destPort = toPort $ map toInteger ((B.unpack . B.take 2 . B.drop 36) header)
              marketType = (B.take 12 . B.drop 5) content
              issueSeqNo = (B.take 3. B.drop 17) content
              quoteAcceptTime = read $ (C8.unpack . B.take 8 . B.drop 206) content :: Integer

printPacket :: Sink QuotePacket IO ()
printPacket = do
    mstr <- await
    case mstr of
      Nothing -> return ()
      Just (quote) -> do
        liftIO $ printQuotePacket quote
        printPacket
