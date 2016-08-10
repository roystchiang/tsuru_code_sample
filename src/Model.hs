module Model where

import Data.ByteString.Char8                    ( unpack )
import Data.ByteString                          ( ByteString )

data QuotePacket = QuotePacket { getMarketType :: ByteString
                               , getIssueSeqNo :: ByteString
                               , getQuoteAcceptTime :: Integer
                               } deriving (Eq, Show)

instance Ord QuotePacket where
    compare x y     = compare (getQuoteAcceptTime x) (getQuoteAcceptTime y)

printQuotePacket :: QuotePacket -> IO ()
printQuotePacket (QuotePacket marketType issueSeqNo quoteAcceptTime)= do
    putStrLn $ "MarketType: " ++ unpack marketType
    putStrLn $ "IssueSeqNo: " ++ unpack issueSeqNo
    putStrLn $ "Quote Accept Time: " ++ show quoteAcceptTime
    putStrLn ""
