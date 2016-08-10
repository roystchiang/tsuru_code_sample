module Model where

import Data.ByteString.Char8                    ( unpack )
import Data.ByteString                          ( ByteString )
import Data.List                                ( intercalate )
import Data.Int                                 ( Int64 )

data Bid = Bid { getBidPrice :: Integer
  , getBidQuantity :: Integer } deriving (Eq)

instance Show Bid where
    show (Bid price quantity) = show price ++ "@" ++ show quantity

data Ask = Ask { getAskPrice :: Integer
               , getAskQuantity :: Integer } deriving (Eq)

instance Show Ask where
    show (Ask price quantity) = show price ++ "@" ++ show quantity

data PacketTime = PacketTime Int64 deriving (Eq)

data QuoteAcceptTime = QuoteAcceptTime Integer deriving (Eq)

instance Show PacketTime where
    show (PacketTime x) = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds ++ ":" ++ show milliseconds
        where milliseconds = (x `div` 1000) `rem` 1000
              seconds = (x `div` (1000*1000)) `rem` 60
              minutes = (x `div` (1000*1000*60)) `rem` 60
              hours = (x `div` (1000*1000*60*60)) `rem` 24

data QuotePacket = QuotePacket { getPacketTime :: PacketTime
                               , getIssueSeqNo :: ByteString
                               , getQuoteAcceptTime :: Integer
                               , getFirstBid :: Bid
                               , getSecondBid :: Bid
                               , getThirdBid :: Bid
                               , getFourthBid :: Bid
                               , getFifthBid :: Bid
                               , getFirstAsk :: Ask
                               , getSecondAsk :: Ask
                               , getThirdAsk :: Ask
                               , getFourthAsk :: Ask
                               , getFifthAsk :: Ask
                               } deriving (Eq, Show)


instance Ord QuotePacket where
    compare x y     = compare (getQuoteAcceptTime x) (getQuoteAcceptTime y)

formatQuoteAcceptTime :: Integer -> String
formatQuoteAcceptTime x = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds ++ ":" ++ show milliseconds
    where milliseconds = x `rem` 100
          seconds = x `div` 100 `rem` 100
          minutes = x `div` 10000 `rem` 100
          hours = x `div` 1000000 `rem` 100

printQuotePacket :: QuotePacket -> IO ()
printQuotePacket (QuotePacket packetTime issueSeqNo quoteAcceptTime firstBid secondBid thirdBid fourthBid fifthBid firstAsk secondAsk thirdAsk fourthAsk fifthAsk) = putStrLn formatted
    where formatted = intercalate " " [ show packetTime
                                      , formatQuoteAcceptTime quoteAcceptTime
                                      , unpack issueSeqNo
                                      , show firstBid
                                      , show secondBid
                                      , show thirdBid
                                      , show fourthBid
                                      , show fifthBid
                                      , show firstAsk
                                      , show secondAsk
                                      , show thirdAsk
                                      , show fourthAsk
                                      , show fifthAsk]
