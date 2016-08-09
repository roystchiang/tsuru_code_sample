module Main where

import qualified Data.ByteString as B           ( drop
                                                , splitAt
                                                , take
                                                , unpack
                                                , ByteString(..) )
import qualified Data.ByteString.Char8 as C8    ( unpack)
import Data.Maybe                               (fromJust)
import Network.Pcap                             ( loopBS
                                                , openOffline
                                                , PcapHandle
                                                , PktHdr )
import System.Environment                       ( getArgs )

import Lib

data QuotePacket = QuotePacket { getMarketType :: B.ByteString
                               , getIssueSeqNo :: B.ByteString
                               , getQuoteAcceptTime :: B.ByteString
                               } deriving (Eq, Show)

instance Ord QuotePacket where
    compare x y     = compare (getQuoteAcceptTime x) (getQuoteAcceptTime y)

main :: IO ()
main = do
    args <- getArgs
    input <- openOffline("mdf-kospi200.20110216-0.pcap")
    if "r" `elem` args
       then putStrLn("sort!")
       else readPcap input

readPcap :: PcapHandle -> IO ()
readPcap input = do
    loopBS input (-1) parsePcapLine
    return ()

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs

parsePcapLine :: PktHdr -> B.ByteString -> IO ()
parsePcapLine hdr bs = do
    let content = toQuotePacket bs
    printPacket content

toPort :: [Integer] -> Integer
toPort (x:[]) = x
toPort (x:xs) = (x * 256) + toPort xs

toQuotePacket :: B.ByteString -> Maybe QuotePacket
toQuotePacket bs
    | (C8.unpack . B.take 5) content /= "B6034"             = Nothing
    | not $ destPort `elem` [15515, 15516]                  = Nothing
    | otherwise                                             = Just $ QuotePacket marketType issueSeqNo quoteAcceptTime
    where (header, content) = B.splitAt 42 bs
          destPort = toPort $ map toInteger ((B.unpack . B.take 2 . B.drop 36) header)
          marketType = (B.take 12 . B.drop 5) content
          issueSeqNo = (B.take 3. B.drop 17) content
          quoteAcceptTime = (B.take 8 . B.drop 206) content

printPacket :: Maybe QuotePacket -> IO ()
printPacket Nothing = return ()
printPacket (Just (QuotePacket marketType issueSeqNo quoteAcceptTime)) = do
    putStrLn $ "MarketType: " ++ C8.unpack marketType
    putStrLn $ "IssueSeqNo: " ++ C8.unpack issueSeqNo
    putStrLn $ "Quote Accept Time: " ++ C8.unpack quoteAcceptTime
    putStrLn ""
    return ()
