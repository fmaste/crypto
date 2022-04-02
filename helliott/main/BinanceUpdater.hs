{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- base.
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Exception (throw)
import Control.Monad (forever, foldM)
import System.Environment
-- async.
import qualified Control.Concurrent.Async as Async
-- bytestring.
import qualified Data.ByteString.Char8 as BS8
-- http-client.
import qualified Network.HTTP.Client as HTTP
-- http-client-tls.
import qualified Network.HTTP.Client.TLS as HTTPS
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as Trans
-- self.
import qualified Network.Binance.API as API
import qualified Network.Binance.API.Types as Types
import qualified Database.Binance as DB

--------------------------------------------------------------------------------

main :: IO ()
main = do
        (apiKey:apiSecret:_) <- getArgs
        -- PostgreSQL Connection String.
        ------------------------------------------------------------------------
        psql <- PSQL.connectPostgreSQL "port=5433 dbname=binance"
        -- Only one connection manager.
        ------------------------------------------------------------------------
        manager <- HTTP.newManager HTTPS.tlsManagerSettings
        api <- API.newApi manager (BS8.pack apiKey) (BS8.pack apiSecret)
        -- Create channel
        ------------------------------------------------------------------------
        oldiesChan <- newChan
        newiesChan <- newChan
        tradesChan <- newChan
{--
        -- Get lastest trade. Where to start from and start the reader.
        ------------------------------------------------------------------------
        (Just (DB.Trade _ _ (Types.Trade firstTid _ _ _ _ _ _))) <- DB.getFirstTrade psql
        print $ "First trade ID: " ++ (show firstTid)
        when (firstTid > 0) $ do
                oldiesAsync <- Async.async (oldiesReader api oldiesChan tradesChan)
                Async.link oldiesAsync
                if firstTid >= 1000
                        then writeChan oldiesChan (firstTid - 1000, 1000)
                        else writeChan oldiesChan (0, fromInteger (firstTid - 1))
--}
        -- Get newest trade. Where to start from and start the reader.
        ------------------------------------------------------------------------
        (Just lastTrade) <- DB.getLastTrade psql
        let lastTradeId = DB.tradeId lastTrade
        print $ "Last trade ID: " ++ (show lastTradeId)
        writeChan newiesChan lastTradeId
        -- Start the newies reader.
        ------------------------------------------------------------------------
        newiesAsync <- Async.async $ forever $ do
                newiesReader api newiesChan tradesChan
        Async.link newiesAsync
        -- Main loop.
        forever $ newiesWriter psql newiesChan tradesChan

newiesReader :: API.API -> (Chan DB.TradeId) -> (Chan [Types.Trade]) -> IO ()
newiesReader api newiesChan tradesChan = do
        lastTid <- readChan newiesChan
        print ("---------- Begin: New getHistoricalTrades ----------"::String)
        (API.Answer (status,headers,_) (Right (Right trades))) <-
                API.getHistoricalTrades api
                        "BTCUSDT"
                        (Just 1000)
                        (Just $ (DB.unTradeId lastTid) + 1)
        print status
        print headers
        print ("---------- End: New getHistoricalTrades ----------"::String)
        case trades of
                [] -> do
                        print ("No new trades!"::String)
                        writeChan newiesChan lastTid
                _ -> do
                        print $ head trades
                        print $ last trades
                        writeChan tradesChan trades

newiesWriter :: PSQL.Connection
             -> (Chan DB.TradeId)
             -> (Chan [Types.Trade])
             -> IO ()
newiesWriter psql newiesChan tradesChan = do
        trades <- readChan tradesChan
        case trades of
                ([]) -> do
                        print ("Empty older trades error!"::String)
                (ts) -> do
                        -- Add latests ID to Chan.
                        maybeTradeId <- Trans.withTransactionSerializable psql $ do
                                foldM
                                        (\_ trade -> do
                                                tradeId <- DB.newTrade psql trade
                                                return (Just tradeId)
                                        )
                                        Nothing
                                        ts
                        case maybeTradeId of
                                -- When finished saving send the last ID.
                                (Just tradeId) -> writeChan newiesChan tradeId
                                _ -> return ()

oldiesReader :: API.API -> (Chan (Integer,Int)) -> (Chan [Types.Trade]) -> IO ()
oldiesReader api newiesChan tradesChan = forever $ do
        (tId, limit) <- readChan newiesChan
        print ("---------- ---------- ---------- ----------"::String)
        (API.Answer (status,headers,_) (Right (Right trades))) <- API.getHistoricalTrades api
                "BTCUSDT" (Just limit) (Just tId)
        print status
        print headers
        writeChan tradesChan trades
        if tId > 0
                then if tId >= 1000
                        then writeChan
                                newiesChan
                                (tId - (toInteger $ length trades), 1000)
                        else writeChan newiesChan (0, fromInteger (tId - 1))
                else do
                        print ("Nothing older, bye!"::String)
                        throw Async.AsyncCancelled

--------------------------------------------------------------------------------

{-- TODO:

BinanceUpdater: ExceptionInLinkedThread (ThreadId 16) user error (Pattern match failure in do expression at main/BinanceUpdater.hs:61:17-70)

BinanceUpdater: ExceptionInLinkedThread (ThreadId 16) Network.Socket.recvBuf: resource vanished (Connection reset by peer)

BinanceUpdater: HttpExceptionRequest Request {
  host                 = "api.binance.com"
  port                 = 443
  secure               = True
  requestHeaders       = [("X-MBX-APIKEY","FgRWgKTkU7zt6EPfrin5ClTakZgaS33qm5DX3ubemUQXNXBQqK53vg6Ww6Rc0ptp")]
  path                 = "/api/v3/historicalTrades"
  queryString          = "symbol=BTCUSDT&limit=1000&fromId=282616353"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}

--}
