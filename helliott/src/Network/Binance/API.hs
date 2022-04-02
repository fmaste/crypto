{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.Binance.API (
          API ()
        , newApi
        , getExchangeInfo
        , Answer (Answer, answerRaw ,answerBody)
        , getHistoricalTrades
        , getOpenOrders
        , getLimitFromHeader
) where

--------------------------------------------------------------------------------

{--

https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#endpoint-security-type

Public Rest API for Binance (2020-03-24)

General API Information

    The base endpoint is: https://api.binance.com
    All endpoints return either a JSON object or array.
    Data is returned in ascending order. Oldest first, newest last.
    All time and timestamp related fields are in milliseconds.

--}

--------------------------------------------------------------------------------

-- base.
import Control.Monad (when)
-- aeson.
import qualified Data.Aeson as Aeson
-- bytestring.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
-- base16-bytestring.
import qualified Data.ByteString.Base16 as Base16
-- case-insensitive.
import qualified Data.CaseInsensitive as CI
-- cryptohash-sha256.
import qualified Crypto.Hash.SHA256 as SHA256
-- http-client.
import qualified Network.HTTP.Client as HTTP
-- time.
import qualified Data.Time as Time
-- http-types.
import qualified Network.HTTP.Types as HTTPTypes
-- self.
import qualified Network.Binance.API.Types as Types

--------------------------------------------------------------------------------

data API = API {
          _httpManager :: HTTP.Manager
        , _apiKey :: BS.ByteString
        , _apiSecret :: BS.ByteString
        , _exchangeInfo :: Types.ExchangeInfo
}

--------------------------------------------------------------------------------

newApi :: HTTP.Manager -> BS.ByteString -> BS.ByteString -> IO API
newApi httpManager ak as = do
        -- Get Exchange info.
        ------------------------------------------------------------------------
        print ("Get Exchange info:"::String)
        response <- doRequest'
                httpManager
                ak as
                "GET" "exchangeInfo"
                [] False
        let body = HTTP.responseBody response
        let (Right exchangeInfo) = Aeson.eitherDecode' body
        print exchangeInfo
        -- Is the server in UTC ???
        ------------------------------------------------------------------------
        print ("Is the server in UTC ???:"::String)
        let tz = Types.exchangeTZ exchangeInfo
        if tz /= "UTC"
                then error $ "Server TZ error: " ++ (show tz)
                else print ("Server TZ OK!: " ++ (show tz))
        -- Check time difference of no more than 10 seconds.
        ------------------------------------------------------------------------
        print ("Check time difference of no more than 10 seconds:"::String)
        let serverTime = Types.exchangeServerTime exchangeInfo
        myTime <- Time.getCurrentTime
        if (Time.diffUTCTime myTime serverTime) >= 10
                then error $ "Server time error:" ++ (show serverTime)
                else print ("Server time OK!: " ++ (show serverTime))
        return $ API httpManager ak as exchangeInfo

--------------------------------------------------------------------------------

getExchangeInfo :: API -> Types.ExchangeInfo
getExchangeInfo = _exchangeInfo

--------------------------------------------------------------------------------

data Answer a = Answer {
          answerRaw :: (HTTPTypes.Status, HTTPTypes.ResponseHeaders, BSL.ByteString)
        , answerBody :: Either (Either String Types.Error) (Either String a)
} deriving Show

--------------------------------------------------------------------------------

-- | Data is returned in ascending order. Oldest first, newest last.
-- For example, if I ask for trade with 286122353 as ID it returns:
-- Trade {tradeId = 286122353, tradePx = 6694.44, tradeQt = 5.0e-2, tradeQuoteQt = 334.722, tradeTime = 2020-04-03 17:41:07.435 UTC, tradeIsBuyerMaker = False, tradeIsBestMatch = True}
-- ...
-- Trade {tradeId = 286123352, tradePx = 6687.59, tradeQt = 2.99e-3, tradeQuoteQt = 19.9958941, tradeTime = 2020-04-03 17:43:01.407 UTC, tradeIsBuyerMaker = False, tradeIsBestMatch = True}
getHistoricalTrades :: API
                    -> BS.ByteString
                    -> (Maybe Int)
                    -> (Maybe Integer)
                    -> IO (Answer [Types.Trade])
getHistoricalTrades api symbol maybeLimit maybeFromId = do
        let query =
                   [ ("symbol",symbol) ]
                ++ (case maybeLimit of
                        Nothing -> []
                        (Just limit) -> if limit < 1 || limit > 1000
                                then error "Historical trades limit"
                                else [ ("limit", BS8.pack $ show limit) ]
                   )
                ++ (case maybeFromId of
                        Nothing -> []
                        (Just fromId) -> [ ("fromId",BS8.pack $ show fromId) ]
                   )
        response <- doRequest
                api
                "GET" "historicalTrades"
                query False
        return $ processReponse response

--------------------------------------------------------------------------------

getOpenOrders :: API -> Time.UTCTime -> IO (Answer [Int])
getOpenOrders api time = do
        response <- doRequest
                api
                "GET" "openOrders"
                [("timestamp",(BS8.pack $ show $ uTCTimeToMilliseconds time))]
                True
        return $ processReponse response

--------------------------------------------------------------------------------

-- Also see:
-- https://hackage.haskell.org/package/connection
-- https://hackage.haskell.org/package/tls

doRequest :: API
          -> BS.ByteString
          -> String
          -> [(BS.ByteString, BS.ByteString)]
          -> Bool
          -> IO (HTTP.Response BSL.ByteString)
doRequest (API manager ak as _) method endpoint query sig = do
        doRequest' manager ak as method endpoint query sig

-- TODO: Use HTTP.responseOpen
doRequest' :: HTTP.Manager
           -> BS.ByteString
           -> BS.ByteString
           -> BS.ByteString
           -> String
           -> [(BS.ByteString, BS.ByteString)]
           -> Bool
           -> IO (HTTP.Response BSL.ByteString)
doRequest' manager ak as method endpoint query sig = do
        when sig (print $ Base16.encode (SHA256.hmac as "hola"))
        when sig (print $ toQueryStringWithSig as query)
        let request = HTTP.defaultRequest
                {
                          HTTP.method = method
                        , HTTP.secure = True
                        , HTTP.host = "api.binance.com"
                        , HTTP.port = 443
                        , HTTP.path = BS8.pack ("/api/v3/" ++ endpoint)
                        , HTTP.queryString = if sig
                                then toQueryStringWithSig as query
                                else toQueryBs query
                        --, HTTP.queryString = toQueryString apiSecretStr query
                        , HTTP.requestHeaders =
                                [
                                        ("X-MBX-APIKEY", ak)
                                ]
                }
        --print request
        response <- HTTP.httpLbs request manager
        return response

processReponse :: Aeson.FromJSON a
               => (HTTP.Response BSL.ByteString)
               -> (Answer a)
processReponse response = let
        status = HTTP.responseStatus response
        headers =HTTP.responseHeaders response
        body = HTTP.responseBody response
        in if (HTTPTypes.statusCode status) /= 200
                then Answer
                        (status, headers, body)
                        (Left $ Aeson.eitherDecode' body)
                else Answer
                        (status, headers, body)
                        (Right $ Aeson.eitherDecode' body)

-- Query string utils.
--------------------------------------------------------------------------------

toQueryBs :: [(BS.ByteString,BS.ByteString)] -> BS.ByteString
toQueryBs query = BS.intercalate "&" (map (\(a,b) -> a <> "=" <> b) query)

toQueryStringWithSig :: BS.ByteString
                     -> [(BS.ByteString,BS.ByteString)]
                     -> BS.ByteString
toQueryStringWithSig apiSecretBs query = do
        let queryBs = toQueryBs query
        let signature = Base16.encode (SHA256.hmac apiSecretBs queryBs)
        (queryBs <> "&signature=" <> signature)

-- Time utils.
--------------------------------------------------------------------------------

uTCTimeToMilliseconds :: Time.UTCTime -> Integer
uTCTimeToMilliseconds time =
        let nominalDiffTime = Time.diffUTCTime
                time
                (Time.UTCTime (Time.fromGregorian 1970 01 01) 0)
        in round $ nominalDiffTime * 1000

--------------------------------------------------------------------------------

getLimitFromHeader :: HTTPTypes.ResponseHeaders
                   -> Types.RateLimit
                   -> (Maybe Int)
getLimitFromHeader headers rateLimit =
        let maybeHeaderValue = lookup
                (CI.mk $ getLimitHeaderName rateLimit)
                headers
        in case maybeHeaderValue of
                Nothing -> Nothing
                (Just bs) -> (Just $ read $ BS8.unpack bs)

getLimitHeaderName :: Types.RateLimit -> BS.ByteString
getLimitHeaderName (Types.RateLimit _ (Types.RateLimitLimit (Types.Second seconds) _)) =
        "X-MBX-USED-WEIGHT-" <> (BS8.pack $ show seconds) <> "S"
getLimitHeaderName (Types.RateLimit _ (Types.RateLimitLimit (Types.Minute minutes) _)) =
        "X-MBX-USED-WEIGHT-" <> (BS8.pack $ show minutes) <> "M"
getLimitHeaderName (Types.RateLimit _ (Types.RateLimitLimit (Types.Hour hours) _)) =
        "X-MBX-USED-WEIGHT-" <> (BS8.pack $ show hours) <> "H"
getLimitHeaderName (Types.RateLimit _ (Types.RateLimitLimit (Types.Day days) _)) =
        "X-MBX-USED-WEIGHT-" <> (BS8.pack $ show days) <> "D"
