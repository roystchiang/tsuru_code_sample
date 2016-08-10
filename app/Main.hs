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
import Network.Pcap                             ( hdrTime )
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


breakBsToQuoteArgs :: B.ByteString -> [Int] -> [B.ByteString]
breakBsToQuoteArgs _ [] = []
breakBsToQuoteArgs bs (x:xs) = B.take x bs : breakBsToQuoteArgs (B.drop x bs) xs

bsToInteger :: B.ByteString -> Integer
bsToInteger bs = read $ C8.unpack bs

bsToBid :: B.ByteString -> B.ByteString -> Bid
bsToBid price quantity = Bid (bsToInteger price) (bsToInteger quantity)

bsToAsk :: B.ByteString -> B.ByteString -> Ask
bsToAsk price quantity = Ask (bsToInteger price) (bsToInteger quantity)

toPacket :: Conduit Packet IO QuotePacket
toPacket = mapMaybe toQuotePacket where
    toQuotePacket (hdr, bs)
        | (C8.unpack . B.take 5) content /= "B6034"             = Nothing
        | not $ destPort `elem` [15515, 15516]                  = Nothing
        | otherwise                                             = Just $ QuotePacket packetTime issueSeqNo quoteAcceptTime firstBid secondBid thirdBid fourthBid fifthBid firstAsk secondAsk thirdAsk fourthAsk fifthAsk
        where (header, content) = B.splitAt 42 bs
              packetTime = PacketTime $ hdrTime hdr
              destPort = toPort $ map toInteger ((B.unpack . B.take 2 . B.drop 36) header)
              args = breakBsToQuoteArgs content [2, 2, 1, 12, 3, 2, 7, 5, 7, 5, 7, 5, 7, 5, 7, 5, 7, 7, 5, 7, 5, 7, 5, 7, 5, 7, 5, 7, 5, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 8]
              issueSeqNo = args !! 4
              quoteAcceptTime = read $ C8.unpack $ args !! 40 :: Integer
              firstBid = bsToBid (args !! 7) (args !! 8)
              secondBid = bsToBid (args !! 9) (args !! 10)
              thirdBid = bsToBid (args !! 11) (args !! 12)
              fourthBid = bsToBid (args !! 13) (args !! 14)
              fifthBid = bsToBid (args !! 15) (args !! 16)
              firstAsk = bsToAsk (args !! 18) (args !! 19)
              secondAsk = bsToAsk (args !! 20) (args !! 21)
              thirdAsk = bsToAsk (args !! 22) (args !! 23)
              fourthAsk = bsToAsk (args !! 24) (args !! 25)
              fifthAsk = bsToAsk (args !! 26) (args !! 27)

printPacket :: Sink QuotePacket IO ()
printPacket = do
    mstr <- await
    case mstr of
      Nothing -> return ()
      Just (quote) -> do
        liftIO $ printQuotePacket quote
        printPacket
