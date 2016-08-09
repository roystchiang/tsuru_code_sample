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
import Data.Maybe                               ( fromJust )
import Network.Pcap                             ( loopBS
                                                , openOffline
                                                , PcapHandle
                                                , PktHdr )
import Network.Pcap.Conduit                     ( sourceOffline
                                                , Packet )
import System.Environment                       ( getArgs )

import Lib

data QuotePacket = QuotePacket { getMarketType :: B.ByteString
                               , getIssueSeqNo :: B.ByteString
                               , getQuoteAcceptTime :: Integer
                               } deriving (Eq, Show)

instance Ord QuotePacket where
    compare x y     = compare (getQuoteAcceptTime x) (getQuoteAcceptTime y)

main :: IO ()
main = do
    args <- getArgs
    if "r" `elem` args
       then sourceOffline("mdf-kospi200.20110216-0.pcap") $$ (toPacket =$= reorderPacket)=$ printPacket
       else sourceOffline("mdf-kospi200.20110216-0.pcap") $$ toPacket =$ printPacket

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y      = x:(merge xs (y:ys))
  | otherwise   = y:(merge (x:xs) ys)

toPort :: [Integer] -> Integer
toPort (x:[]) = x
toPort (x:xs) = (x * 256) + toPort xs


reorderPacket :: Conduit QuotePacket IO QuotePacket
reorderPacket = loop []
    where yieldItem x = do
              if length x == 1
                  then yield $ head x
                  else do
                      yield $ head x
                      yieldItem $ tail x
          loop xs = do
              x <- await
              case x of
                  Nothing -> yieldItem xs
                  Just x -> do
                      {-let sorted = quicksort (xs ++ [x])-}
                      let sorted = merge xs [x]
                      if getQuoteAcceptTime (head sorted) + 300 < getQuoteAcceptTime x
                          then do
                              yield $ head sorted
                              loop $ tail sorted
                          else loop sorted

toQuotePacket' :: B.ByteString -> Maybe QuotePacket
toQuotePacket' bs
    | (C8.unpack . B.take 5) content /= "B6034"             = Nothing
    | not $ destPort `elem` [15515, 15516]                  = Nothing
    | otherwise                                             = Just $ QuotePacket marketType issueSeqNo quoteAcceptTime
    where (header, content) = B.splitAt 42 bs
          destPort = toPort $ map toInteger ((B.unpack . B.take 2 . B.drop 36) header)
          marketType = (B.take 12 . B.drop 5) content
          issueSeqNo = (B.take 3. B.drop 17) content
          quoteAcceptTime = read $ (C8.unpack . B.take 8 . B.drop 206) content :: Integer

toQuotePacket :: Packet -> Maybe QuotePacket
toQuotePacket (hdr, bs) = toQuotePacket' bs

toPacket :: Conduit Packet IO QuotePacket
toPacket = mapMaybe toQuotePacket

printPacket :: Sink QuotePacket IO ()
printPacket = do
    mstr <- await
    case mstr of
      Nothing -> return ()
      Just (QuotePacket marketType issueSeqNo quoteAcceptTime) -> do
        liftIO $ putStrLn $ "MarketType: " ++ C8.unpack marketType
        liftIO $ putStrLn $ "IssueSeqNo: " ++ C8.unpack issueSeqNo
        liftIO $ putStrLn $ "Quote Accept Time: " ++ show quoteAcceptTime
        liftIO $ putStrLn ""
        printPacket
