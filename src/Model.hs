module Model
    ( bsToInteger
    , bsToQuote
    , PacketTime(..)
    , QuotePacket(..) )where

import Data.ByteString.Char8                    ( readInteger
                                                , unpack )
import Data.ByteString                          ( ByteString )
import Data.List                                ( intercalate )
import Data.Int                                 ( Int64 )

type Bid = Quote
type Ask = Quote

data Quote = Quote
    { getBidPrice :: Integer
    , getBidQuantity :: Integer
    } deriving (Eq)

instance Show Quote where
    show (Quote price quantity) = show price ++ "@" ++ show quantity

data PacketTime = PacketTime Int64 deriving (Eq)

instance Show PacketTime where
    show (PacketTime x) = intercalate ":" $ [ show hours
                                            , show minutes
                                            , show seconds
                                            , show milliseconds
                                            ]
        where milliseconds = x `div` 1000 `rem` 1000
              seconds = x `div` (1000*1000) `rem` 60
              minutes = x `div` (1000*1000*60) `rem` 60
              hours = x `div` (1000*1000*60*60) `rem` 24

data QuotePacket = QuotePacket
    { getPacketTime :: PacketTime
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
    } deriving (Eq)

instance Show QuotePacket where
    show (QuotePacket packetTime issueCode quoteAcceptTime firstBid secondBid thirdBid fourthBid fifthBid firstAsk secondAsk thirdAsk fourthAsk fifthAsk) = formatted
        where formatted = intercalate " " $ [ show packetTime
                                            , formatQuoteAcceptTime quoteAcceptTime
                                            , issueCode
                                            ] ++ map show
                                            [ firstBid
                                            , secondBid
                                            , thirdBid
                                            , fourthBid
                                            , fifthBid
                                            , firstAsk
                                            , secondAsk
                                            , thirdAsk
                                            , fourthAsk
                                            , fifthAsk
                                            ]
instance Ord QuotePacket where
    compare x y     = compare (getQuoteAcceptTime x) (getQuoteAcceptTime y)

bsToInteger :: ByteString -> Integer
bsToInteger bs = fromMaybe $ readInteger bs
    where fromMaybe Nothing = 0
          fromMaybe (Just (x, _)) = x

bsToQuote :: ByteString -> ByteString -> Quote
bsToQuote price quantity = Quote (bsToInteger price) (bsToInteger quantity)

formatQuoteAcceptTime :: Integer -> String
formatQuoteAcceptTime x = intercalate ":" $ [ show hours
                                            , show minutes
                                            , show seconds
                                            , show milliseconds
                                            ]
    where milliseconds = x `rem` 100
          seconds = x `div` 100 `rem` 100
          minutes = x `div` (100*100) `rem` 100
          hours = x `div` (100*100*100) `rem` 100
