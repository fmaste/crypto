{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Database.Binance (
          TradeId (unTradeId)
        , newTrade
        , Trade (
                  tradeId, tradePx, tradeQt, tradeQuoteQt
                , tradeTime, tradeIsBuyerMaker, tradeIsBestMatch
        )
        , getFirstTrade, getLastTrade
) where

--------------------------------------------------------------------------------

-- base.
--
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromField as FromField
import qualified Database.PostgreSQL.Simple.ToField as ToField
import qualified Database.PostgreSQL.Simple.FromRow as FromRow
-- time.
import qualified Data.Time as Time
-- self.
import qualified Network.Binance.API.Types as Types

--------------------------------------------------------------------------------

newtype TradeId = TradeId {unTradeId :: Integer}
        deriving (Eq, Show)

instance FromField.FromField TradeId where
        fromField field mdata = do
                x <- FromField.fromField field mdata
                return (TradeId x)

instance ToField.ToField TradeId where
        toField (TradeId x) = ToField.toField x

newTrade :: PSQL.Connection
         -> Types.Trade
         -> IO TradeId
newTrade psql trade = do
        [(PSQL.Only tId)] <- PSQL.query psql
                (" INSERT INTO public.trades "
                        <> " ( "
                                <> "   trade_id "
                                <> " , trade_px "
                                <> " , trade_qt "
                                <> " , trade_quote_qt "
                                <> " , trade_time "
                                <> " , trade_buyer_maker "
                                <> " , trade_best_match "
                        <> " ) "
                        <> " VALUES (?,?,?,?,?,?,?) "
                        <> " RETURNING trade_id "
                        <> " ; "
                )
                (
                          Types.tradeId trade
                        , Types.tradePx trade
                        , Types.tradeQt trade
                        , Types.tradeQuoteQt trade
                        , Types.tradeTime trade
                        , Types.tradeIsBuyerMaker trade
                        , Types.tradeIsBestMatch trade
                )
        return tId

--------------------------------------------------------------------------------

data Trade = Trade {
          tradeId :: TradeId
        , tradePx :: Rational
        , tradeQt :: Rational
        , tradeQuoteQt :: Rational
        , tradeTime :: Time.UTCTime
        , tradeIsBuyerMaker :: Bool
        , tradeIsBestMatch :: Bool
} deriving (Show, Eq)

instance FromRow.FromRow Trade where
        fromRow = do
                -- "trade_id"
                tId <- FromRow.field
                -- "trade_px"
                tPx <- FromRow.field
                -- "trade_qt"
                tQty <- FromRow.field
                -- "trade_quote_qt"
                tQuoteQty <- FromRow.field
                -- "trade_time"
                tTime <- FromRow.field
                -- "trade_buyer_maker"
                tIsBuyerMaker <- FromRow.field
                -- "trade_best_match"
                tIsBestMatch <- FromRow.field
                return $ Trade
                        tId
                        tPx
                        tQty
                        tQuoteQty
                        tTime
                        tIsBuyerMaker
                        tIsBestMatch

getFirstTrade :: PSQL.Connection
              -> IO (Maybe Trade)
getFirstTrade psql = do
        ans <- PSQL.query_ psql
                ("SELECT * "
                        <> " FROM public.trades "
                        <> " ORDER BY trade_id ASC "
                        <> " LIMIT 1 "
                        <> ";"
                )
        case ans of
                [] -> return Nothing
                (row:_) -> return $ Just row

getLastTrade :: PSQL.Connection
             -> IO (Maybe Trade)
getLastTrade psql = do
        ans <- PSQL.query_ psql
                ("SELECT * "
                        <> " FROM public.trades "
                        <> " ORDER BY trade_id DESC "
                        <> " LIMIT 1 "
                        <> ";"
                )
        case ans of
                [] -> return Nothing
                (row:_) -> return $ Just row
