module Model where

import Data.ByteString.Char8                    ( unpack )
import Data.ByteString                          ( ByteString )
import Data.List                                ( intercalate )
import Data.Int                                 ( Int64 )

data Quote = Quote { getBidPrice :: Integer
                   , getBidQuantity :: Integer } deriving (Eq)

type Bid = Quote

type Ask = Quote

instance Show Quote where
    show (Quote price quantity) = show price ++ "@" ++ show quantity


data PacketTime = PacketTime Int64 deriving (Eq)

instance Show PacketTime where
    show (PacketTime x) = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds ++ ":" ++ show milliseconds
        where milliseconds = (x `div` 1000) `rem` 1000
              seconds = (x `div` (1000*1000)) `rem` 60
              minutes = (x `div` (1000*1000*60)) `rem` 60
              hours = (x `div` (1000*1000*60*60)) `rem` 24

data QuotePacket = QuotePacket { getPacketTime :: PacketTime
                               , getIssueCode :: String
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
printQuotePacket (QuotePacket packetTime issueCode quoteAcceptTime firstBid secondBid thirdBid fourthBid fifthBid firstAsk secondAsk thirdAsk fourthAsk fifthAsk) = putStrLn formatted
    where formatted = intercalate " " $ [ show packetTime , formatQuoteAcceptTime quoteAcceptTime , issueCode] ++ map show [firstBid, secondBid, thirdBid, fourthBid, fifthBid, firstAsk, secondAsk, thirdAsk, fourthAsk]
