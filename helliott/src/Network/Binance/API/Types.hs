{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

{--

Public Rest API for Binance (2020-03-24)
https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md

Data is returned in ascending order. Oldest first, newest last.
All time and timestamp related fields are in milliseconds.

--}

module Network.Binance.API.Types (
          Error (statusCode, statusMessage)
        , ExchangeInfo (exchangeTZ, exchangeServerTime, exchangeRateLimits)
        , RateLimit (RateLimit, rateLimitType, rateLimitLimit)
        , RateLimitType (RawRequestsRateLimit, RequestsWeightRateLimit, OrdersRateLimit)
        , RateLimitLimit (RateLimitLimit, rateLimitInterval, rateLimitRequests)
        , RateLimitInterval (Second, Minute, Hour, Day)
        , TickerPrice (tickerPriceSymbol, tickerPricePx)
        -- Nobody else can create a Trade because the provided Scientific number
        -- could diverge and consume all space when converting to Rational.
        -- This way only it can only happen if a very large string containing
        -- the number is returned on the JSON encoded HTTP response.
        , Trade (
                  tradeId, tradePx, tradeQt, tradeQuoteQt
                , tradeTime, tradeIsBuyerMaker, tradeIsBestMatch
        )
) where

--------------------------------------------------------------------------------

-- base.
--
-- aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
-- scientific.
import qualified Data.Scientific as Scientific
-- text.
import qualified Data.Text as Text
-- time.
import qualified Data.Time as Time

--------------------------------------------------------------------------------

data Error = Error {
          statusCode :: Int
        , statusMessage :: Text.Text
} deriving Show

instance Aeson.FromJSON Error where
        parseJSON v = Aeson.withObject "Error" 
                (\o -> do
                        -- Found this on an unsigned message (ticker price).
                        mscLong  <- o Aeson..:? "statusCode"
                        msmLong  <- o Aeson..:? "statusMessage"
                        -- Found this on a signed message (openOrders).
                        mscShort <- o Aeson..:? "code"
                        msmShort <- o Aeson..:? "msg"
                        sc <- case mscLong of
                                (Just sc) -> return sc
                                Nothing ->  case mscShort of
                                        (Just sc) -> return sc
                                        Nothing -> (
                                                        AesonTypes.typeMismatch
                                                        "No status message"
                                                        v
                                                    )
                        sm <- case msmLong of
                                (Just sm) -> return sm
                                Nothing ->  case msmShort of
                                        (Just sm) -> return sm
                                        Nothing -> (
                                                        AesonTypes.typeMismatch
                                                        "No status code"
                                                        v
                                                    )
                        return $ Error sc sm
                ) v

--------------------------------------------------------------------------------

data ExchangeInfo = ExchangeInfo {
        -- The timezone.
          exchangeTZ :: Text.Text
        -- The time.
        , exchangeServerTime :: Time.UTCTime
        -- The limits on each API category.
        , exchangeRateLimits :: [RateLimit]
        -- TODO:
        , exchangeSymbols :: [Int]
} deriving (Show, Eq)

instance Aeson.FromJSON ExchangeInfo where
        parseJSON = Aeson.withObject "ExchangeInfo" $ \v -> do
                tz <- v Aeson..: "timezone"
                time <- v Aeson..: "serverTime"
                limits <- v Aeson..: "rateLimits"
                -- TODO: Symbols.
                return $ ExchangeInfo tz (millisecondsToUTCTime time) limits []

--------------------------------------------------------------------------------

{-- Example limits:

exchangeRateLimits = [
         fromList [
                 ("rateLimitType",String "REQUEST_WEIGHT")
                ,("interval",String "MINUTE")
                ,("limit",Number 1200.0)
                ,("intervalNum",Number 1.0)
        ]
        ,fromList [
                 ("rateLimitType",String "ORDERS")
                ,("interval",String "SECOND")
                ,("limit",Number 100.0)
                ,("intervalNum",Number 10.0)
        ]
        ,fromList [
                 ("rateLimitType",String "ORDERS")
                ,("interval",String "DAY")
                ,("limit",Number 200000.0)
                ,("intervalNum",Number 1.0)
        ]
]

--}

data RateLimit = RateLimit {
          rateLimitType :: RateLimitType
        , rateLimitLimit :: RateLimitLimit
} deriving (Show, Eq)

data RateLimitType =
        RawRequestsRateLimit | RequestsWeightRateLimit | OrdersRateLimit
        deriving (Show, Eq)

-- Interval type and amount of intervals.
-- For example (Minute 5) means every 5 minutes.
data RateLimitInterval = Second Int | Minute Int | Hour Int | Day Int
        deriving (Show, Eq)

data RateLimitLimit = RateLimitLimit {
          -- The type and amount of intervals.
          rateLimitInterval :: RateLimitInterval
          -- The amount of requests.
        , rateLimitRequests :: Int
} deriving (Show, Eq)

instance Aeson.FromJSON RateLimit where
        parseJSON v = Aeson.withObject "RateLimits"
                (\o -> do
                        -- "RAW_REQUEST", "REQUEST_WEIGHT" or "ORDERS"
                        typ <- o Aeson..: "rateLimitType"
                        -- SECOND, MINUTE, HOUR or DAY.
                        int <- o Aeson..: "interval"
                        -- Number of intervals.
                        num <- o Aeson..: "intervalNum"
                        -- Number of requests.
                        lim <- o Aeson..: "limit"
                        intType <- if int == ("SECOND" :: Text.Text)
                                then (return $ Second num)
                                else if int == "MINUTE"
                                        then (return $ Minute num)
                                        else if int == "HOUR"
                                                then (return $ Hour num)
                                                else if int == "DAY"
                                                        then (return $ Day num)
                                                        else (
                                                                AesonTypes.typeMismatch
                                                                "Invalid interval"
                                                                v
                                                            )
                        let rtt = RateLimitLimit intType lim
                        if typ == ("RAW_REQUEST" :: Text.Text)
                                then (return $ RateLimit RawRequestsRateLimit rtt)
                                else if typ == "REQUEST_WEIGHT"
                                        then (return $ RateLimit RequestsWeightRateLimit rtt)
                                        else if typ == "ORDERS"
                                                then (return $ RateLimit OrdersRateLimit rtt)
                                                else (
                                                                AesonTypes.typeMismatch
                                                                "Invalid rate limiter" 
                                                                v
                                                        )
                )
                v

--------------------------------------------------------------------------------

data TickerPrice = TickerPrice {
          tickerPriceSymbol :: Text.Text
        , tickerPricePx :: Scientific.Scientific
} deriving (Show, Eq)

instance Aeson.FromJSON TickerPrice where
        parseJSON = Aeson.withObject "TickerPrice" $ \v -> do
                ticker <- v Aeson..: "symbol"
                priceText <- v Aeson..: "price"
                return $ TickerPrice ticker (read (Text.unpack priceText))

--------------------------------------------------------------------------------

data Trade = Trade {
          tradeId :: Integer
        , tradePx :: Scientific.Scientific
        , tradeQt :: Scientific.Scientific
        , tradeQuoteQt :: Scientific.Scientific
        , tradeTime :: Time.UTCTime
        , tradeIsBuyerMaker :: Bool
        , tradeIsBestMatch :: Bool
} deriving (Show, Eq)

instance Aeson.FromJSON Trade where
        parseJSON = Aeson.withObject "Trade" $ \v -> do
                tId <- v Aeson..: "id"
                tPx <- v Aeson..: "price"
                tQty <- v Aeson..: "qty"
                tQQty <- v Aeson..: "quoteQty"
                tTime <- v Aeson..: "time"
                tM <- v Aeson..: "isBuyerMaker"
                tB <- v Aeson..: "isBestMatch"
                return $ Trade
                        tId
                        (read (Text.unpack tPx))
                        (read (Text.unpack tQty))
                        (read (Text.unpack tQQty))
                        (millisecondsToUTCTime tTime)
                        tM
                        tB

-- Time utils.
--------------------------------------------------------------------------------

millisecondsToUTCTime :: Integer -> Time.UTCTime
millisecondsToUTCTime millis =
        Time.addUTCTime
                (Time.secondsToNominalDiffTime $ (fromIntegral millis) / 1000)
                (Time.UTCTime (Time.fromGregorian 1970 01 01) 0)
