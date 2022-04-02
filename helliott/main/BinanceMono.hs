{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- base.
import Control.Monad (forever)
-- async.
import qualified Control.Concurrent.Async as Async
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as Trans
-- time.
import qualified Data.Time as Time
-- self.
import qualified Database.Binance as BinanceDB
import qualified Database.Helliott as HelliottDB

--------------------------------------------------------------------------------

{--
fromRational has been applied to a repeating decimal which can't be represented
as a Scientific! It's better to avoid performing fractional operations on
Scientifics and convert them to other fractional types like Double as early as
possible.
--}

-- Used to collect the data of every trade that belongs to the same monowave.
data MonoWaveReader = MonoWaveReader {
        -- A previous monowave if this is not the first.
          monoWaveReaderLastWave :: (Maybe HelliottDB.MonoWave)
        -- How many trades were read.
        , monoWaveReaderCount :: Int
        -- Every Trade ID read.
        , monoWaveReaderTradeIds :: [BinanceDB.TradeId]
        -- The time of the first trade read.
        , monoWaveReaderTimeStart :: (Maybe Time.UTCTime)
        -- The time of the last read trade.
        , monoWaveReaderTimeEnd :: (Maybe Time.UTCTime)
        -- The price of the first trade read.
        , monoWaveReaderPriceStart :: (Maybe Rational)
        -- The price of the last read trade.
        , monoWaveReaderPriceEnd :: (Maybe Rational)
        -- Track sum of volumes of every trade read.
        , monoWaveReaderSumVol :: Rational
        -- Track sum of amounts of every trade read.
        , monoWaveReaderSumAmount :: Rational
        -- Track sum of prices of every trade read.
        , monoWaveReaderSumPx :: Rational
        -- Track sum of prices multiplied by its by volume of every trade read.
        , monoWaveReaderSumPxVw :: Rational
        -- Track sum of prices multiplied by its by amount of every trade read.
        , monoWaveReaderSumPxAw :: Rational
} deriving Show

newMonoWaveReader :: (Maybe HelliottDB.MonoWave) -> MonoWaveReader
newMonoWaveReader maybeMwr = MonoWaveReader
        maybeMwr
        0
        []
        Nothing Nothing
        Nothing Nothing
        0 0 0 0 0

-- Add the data of a new trade.
addTrade :: MonoWaveReader -> BinanceDB.Trade -> MonoWaveReader
addTrade mwr trade = mwr {
        -- Adds one more trade.
          monoWaveReaderCount = (monoWaveReaderCount mwr) + 1
        -- Add trade ID for later storing the relationship on the DB.
        , monoWaveReaderTradeIds =    (monoWaveReaderTradeIds mwr)
                                   ++ [(BinanceDB.tradeId trade)]
        -- If no first time this is the first trade of the wave.
        , monoWaveReaderTimeStart = case (monoWaveReaderTimeStart mwr) of
                maybeTime@(Just _) -> maybeTime
                Nothing -> (Just (BinanceDB.tradeTime trade))
        -- If a first time exists this is the latests trade of the wave.
        , monoWaveReaderTimeEnd = case (monoWaveReaderTimeStart mwr) of
                (Just _) -> (Just (BinanceDB.tradeTime trade))
                Nothing -> Nothing
        -- If no first price this is the first trade of the wave.
        , monoWaveReaderPriceStart = case (monoWaveReaderPriceStart mwr) of
                maybePrice@(Just _) -> maybePrice
                Nothing -> (Just (BinanceDB.tradePx trade))
        -- If a first price exists this is the latests trade of the wave.
        , monoWaveReaderPriceEnd = case (monoWaveReaderPriceStart mwr) of
                (Just _) -> (Just (BinanceDB.tradePx trade))
                Nothing -> Nothing
        -- Sum of volumes.
        , monoWaveReaderSumVol =
                  (monoWaveReaderSumVol mwr)
                + (BinanceDB.tradeQt trade)
        -- Sum of amounts.
        , monoWaveReaderSumAmount =
                  (monoWaveReaderSumAmount mwr)
                + (BinanceDB.tradeQuoteQt trade)
        -- Sum of prices.
        , monoWaveReaderSumPx =
                  (monoWaveReaderSumPx mwr)
                + (BinanceDB.tradePx trade)
        -- Sum of prices multiplied by quantity.
        , monoWaveReaderSumPxVw =
                  (monoWaveReaderSumPxVw mwr)
                + ((BinanceDB.tradePx trade) * (BinanceDB.tradeQt trade))
        -- Sum of prices multiplied by amount.
        , monoWaveReaderSumPxAw =
                  (monoWaveReaderSumPxAw mwr)
                + ((BinanceDB.tradePx trade) * (BinanceDB.tradeQuoteQt trade))
}

isSameMonoWavePrices3 :: Rational -> Rational -> Rational -> Bool
isSameMonoWavePrices3 px1 px2 px3 =
        if ((px1 >= px2) && (px2 >= px3)) || ((px1 <= px2) && (px2 <= px3))
                then True
                else False

isSameMonoWavePrices4 :: Rational -> Rational -> Rational -> Rational -> Bool
isSameMonoWavePrices4 px1 px2 px3 px4 =
        if ((px1 >= px2) && (px3 >= px4)) || ((px1 <= px2) && (px3 <= px4))
                then True
                else False

-- Decide if this price is part of the actual monowave being read.
isSameMonoWave :: MonoWaveReader -> Rational -> Bool
isSameMonoWave mwr px = case (monoWaveReaderLastWave mwr) of
        -- A previous wave exists, this is not the first one.
        (Just lastWave) -> case (monoWaveReaderPriceStart mwr) of
                -- At least one trade was read.
                (Just firstPrice) -> case (monoWaveReaderPriceEnd mwr) of
                        -- At least two trades were read.
                        (Just lastPrice) ->
                                isSameMonoWavePrices4
                                        (snd $ HelliottDB.monoWavePx lastWave)
                                        firstPrice
                                        lastPrice
                                        px
                        -- Only one trades were read.
                        Nothing -> isSameMonoWavePrices3
                                (snd $ HelliottDB.monoWavePx lastWave)
                                firstPrice
                                px
                -- No trade was read.
                Nothing -> True
        -- There is no previous wave, this is the first one.
        _ -> case (monoWaveReaderPriceStart mwr) of
                -- At least one trade was read.
                (Just firstPrice) -> case (monoWaveReaderPriceEnd mwr) of
                        -- At least two trades were read.
                        (Just lastPrice) ->
                                isSameMonoWavePrices3 firstPrice lastPrice px
                        -- We read only one trade.
                        Nothing -> True
                -- We have no previous trade.
                Nothing -> True

getMonoWaveTimeRange :: MonoWaveReader -> (Time.UTCTime, Time.UTCTime)
getMonoWaveTimeRange mwr = case (monoWaveReaderLastWave mwr) of
        -- A previous wave exists, this is not the first one.
        (Just lastWave) -> case (monoWaveReaderTimeEnd mwr) of
                -- At least two trades were read.
                (Just lastTime) ->
                        (
                                  (snd $ HelliottDB.monoWaveTime lastWave)
                                , lastTime
                        )
                -- Only one trade was read, a one price point wave.
                Nothing -> case (monoWaveReaderTimeStart mwr) of
                        (Just firstTime) ->
                                (
                                          (snd $ HelliottDB.monoWaveTime lastWave)
                                        , firstTime
                                )
                        -- This wave has no first trade ???
                        _ -> error ""
        -- There is no previous wave, this is the first one.
        _ -> case (monoWaveReaderTimeStart mwr) of
                -- Look for the first time found.
                (Just timeStart) -> case (monoWaveReaderTimeEnd mwr) of
                        -- The first and last time of the current wave.
                        (Just lastTime) -> (timeStart, lastTime)
                        -- It's an error if the first wave has only one trade.
                        _ -> error ""
                -- It's an error if no trades were read.
                _ -> error ""

getMonoWavePriceRange :: MonoWaveReader -> (Rational, Rational)
getMonoWavePriceRange mwr = case (monoWaveReaderLastWave mwr) of
        -- A previous wave exists, this is not the first one.
        (Just lastWave) -> case (monoWaveReaderPriceEnd mwr) of
                -- At least two trades were read.
                (Just lastPrice) ->
                        (
                                  (snd $ HelliottDB.monoWavePx lastWave)
                                , lastPrice
                        )
                -- Only one trade was read, a one price point wave.
                Nothing -> case (monoWaveReaderPriceStart mwr) of
                        (Just firstPrice) ->
                                (
                                          (snd $ HelliottDB.monoWavePx lastWave)
                                        , firstPrice
                                )
                        -- This wave has no first trade ???
                        _ -> error ""
        -- There is no previous wave, this is the first one.
        _ -> case (monoWaveReaderPriceStart mwr) of
                -- Look for the first price found.
                (Just priceStart) -> case (monoWaveReaderPriceEnd mwr) of
                        -- The first and last price of the current wave.
                        (Just lastPrice) -> (priceStart, lastPrice)
                        -- It's an error if the first wave has only one trade.
                        _ -> error ""
                -- It's an error if no trades were read.
                _ -> error ""

--------------------------------------------------------------------------------

main :: IO ()
main = do
        -- PostgreSQL Connection String.
        ------------------------------------------------------------------------
        psql <- PSQL.connectPostgreSQL "port=5433 dbname=binance"
        ------------------------------------------------------------------------
        forever $ Trans.withTransactionLevel Trans.Serializable psql $ do
                -- Get last monowave.
                print ("---------- Begin: getLastMonoWave ----------"::String)
                maybeLastMonoWave <- HelliottDB.getLastMonoWave psql
                print maybeLastMonoWave
                print ("---------- End: getLastMonoWave ----------"::String)
                print ("---------- Begin: Create monowave ----------"::String)
                eitherMwr <- HelliottDB.getUnusedTrades psql
                        (\(mwr,_) trade ->
                                let tradePx = (BinanceDB.tradePx trade)
                                in if isSameMonoWave mwr tradePx
                                        -- If price is on the same monowave add
                                        -- this trade and ask for more input.
                                        then (Right (addTrade mwr trade, False))
                                        -- No more input, finish.
                                        else (Left (mwr, True))
                        )
                        (newMonoWaveReader maybeLastMonoWave, False)
                print eitherMwr
                print ("---------- End: Create monowave ----------"::String)
                case eitherMwr of
                        -- Returned because of no more input.
                        (_, False) -> return ()
                        -- We signaled the end of a monowave.
                        (mwr, True) -> do
                                print ("---------- Begin: Save monowave ----------"::String)
                                mwi <- saveMonoWave psql mwr
                                print mwi
                                print ("---------- End: Save monowave ----------"::String)

saveMonoWave :: PSQL.Connection -> MonoWaveReader -> IO HelliottDB.MonoWaveId
saveMonoWave psql mwr = do
        -- Save the new monowave.
        let order = case (monoWaveReaderLastWave mwr) of
                Nothing -> 0
                (Just mw) -> ((HelliottDB.unMonoWaveId $ HelliottDB.monoWaveId mw) + 1)
        let count = monoWaveReaderCount mwr
        let volume = monoWaveReaderSumVol mwr
        let amount = monoWaveReaderSumAmount mwr
        print (getMonoWaveTimeRange mwr)
        print (getMonoWavePriceRange mwr)
        -- Store monowave.
        monoWaveId <- HelliottDB.newMonoWave psql
                order
                (getMonoWaveTimeRange mwr)
                (getMonoWavePriceRange mwr)
                count
                volume
                amount
                ( (monoWaveReaderSumPx mwr) / (toRational count) )
                ( (monoWaveReaderSumPxVw mwr) / volume )
                ( (monoWaveReaderSumPxAw mwr) / amount )
        -- Add all the monowave-trade relationships.
        _ <- mapM
                (\tradeId -> do
                        HelliottDB.newMonoWaveTrade psql
                                tradeId monoWaveId
                )
                (monoWaveReaderTradeIds mwr)
        return monoWaveId
