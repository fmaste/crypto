{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Database.Helliott (
          MonoWaveId (unMonoWaveId)
        , MonoWave (
                  monoWaveId, monoWaveTime, monoWavePx, monoWaveCount
                , monoWaveVolume, monoWaveAmount, monoWavePxAvg, monoWavePxVwavg
        )
        , getLastMonoWave
        , getUnusedTrades
        , newMonoWave
        , newMonoWaveTrade
) where

--------------------------------------------------------------------------------

-- base.
import Control.Monad (foldM)
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromField as FromField
import qualified Database.PostgreSQL.Simple.FromRow as FromRow
import qualified Database.PostgreSQL.Simple.Range as PGR
import qualified Database.PostgreSQL.Simple.ToField as ToField
-- scientific.
import qualified Data.Scientific as Scientific
-- time.
import qualified Data.Time as Time
-- self.
import qualified Database.Binance as BinanceDB

--------------------------------------------------------------------------------

fuckingScientific :: Rational -> Scientific.Scientific
fuckingScientific r =  case (Scientific.fromRationalRepetend (Just 6) r) of
        (Left (sc,_)) -> sc
        (Right (sc,_)) -> sc

--------------------------------------------------------------------------------

newtype MonoWaveId = MonoWaveId {unMonoWaveId :: Integer}
        deriving (Eq, Show)

instance FromField.FromField MonoWaveId where
        fromField field mdata = do
                x <- FromField.fromField field mdata
                return (MonoWaveId x)

instance ToField.ToField MonoWaveId where
        toField (MonoWaveId x) = ToField.toField x

{--
data TimeRange =
        -- This it the n+1 wave ( (from,to] ).
          TimePrev Time.UTCTime Time.UTCTime
        -- This it the first wave ( [from,to] ).
        | TimeFirst Time.UTCTime Time.UTCTime
        deriving Show

instance FromField.FromField TimeRange where
        fromField field mdata = do
                x <- FromField.fromField field mdata
                case x of
                        (PGR.PGRange (PGR.Inclusive px1) (PGR.Inclusive px2)) ->
                                (return $ TimeFirst px1 px2)
                        (PGR.PGRange (PGR.Exclusive px1) (PGR.Inclusive px2)) ->
                                (return $ TimePrev px1 px2)
                        _ -> error "MonoWave time range not in expected range."

instance ToField.ToField TimeRange where
        toField (TimePrev t1 t2) = ToField.toField $ PGR.PGRange
                (PGR.Exclusive t1)
                (PGR.Inclusive t2)
        toField (TimeFirst t1 t2) = ToField.toField $ PGR.PGRange
                (PGR.Inclusive t1)
                (PGR.Inclusive t2)

data PriceRange =
        -- This it the n+1 wave ( (from,to] ).
          PricePrev Rational Rational
        -- This it the first wave ( [from,to] ).
        | PriceFirst Rational Rational
        deriving Show

instance FromField.FromField PriceRange where
        fromField field mdata = do
                x <- FromField.fromField field mdata
                case x of
                        (PGR.PGRange (PGR.Inclusive px1) (PGR.Inclusive px2)) ->
                                (return $ PriceFirst px1 px2)
                        (PGR.PGRange (PGR.Exclusive px1) (PGR.Inclusive px2)) ->
                                (return $ PricePrev px1 px2)
                        _ -> error "MonoWave price range not in expected range."

instance ToField.ToField PriceRange where
        toField (PricePrev px1 px2) = ToField.toField $ PGR.PGRange
                (PGR.Exclusive $ fuckingScientific px1)
                (PGR.Inclusive $ fuckingScientific px2)
        toField (PriceFirst px1 px2) = ToField.toField $ PGR.PGRange
                (PGR.Inclusive $ fuckingScientific px1)
                (PGR.Inclusive $ fuckingScientific px2)
--}

data MonoWave = MonoWave {
          monoWaveId :: MonoWaveId
        , monoWaveTime :: (Time.UTCTime, Time.UTCTime)
        , monoWavePx :: (Rational, Rational)
        , monoWaveCount :: Int
        , monoWaveVolume :: Rational
        , monoWaveAmount :: Rational
        , monoWavePxAvg :: Rational
        , monoWavePxVwavg :: Rational
        , monoWavePxAwavg :: Rational
} deriving Show

instance FromRow.FromRow MonoWave where
        fromRow = do
                -- "id"
                monowave_id <- FromRow.field
                -- time_range.
                time_range <- FromRow.field
                -- Wave is going up ?
                up <- FromRow.field
                -- px_range.
                px_range <- FromRow.field
                -- "trades_count"
                trades_count <- FromRow.field
                -- "volume"
                volume <- FromRow.field
                -- "amount"
                amount <- FromRow.field
                -- "px_avg"
                px_avg <- FromRow.field
                -- "px_vwavg"
                px_vwavg <- FromRow.field
                -- "px_awavg"
                px_awavg <- FromRow.field
                return $ MonoWave
                        monowave_id
                        (case time_range of
                                (PGR.PGRange (PGR.Exclusive t1) (PGR.Inclusive t2)) ->
                                        (t1, t2)
                                (PGR.PGRange (PGR.Inclusive t1) (PGR.Inclusive t2)) ->
                                        (t1, t2)
                                _ -> error "MonoWave time range not in expected range."
                        )
                        (case px_range of
                                (PGR.PGRange (PGR.Exclusive p1) (PGR.Inclusive p2)) ->
                                        if up
                                                then (p1, p2)
                                                else (p2, p1)
                                _ -> error "MonoWave price range not in expected range."
                        )
                        trades_count
                        volume
                        amount
                        px_avg
                        px_vwavg
                        px_awavg

getLastMonoWave :: PSQL.Connection -> IO (Maybe MonoWave)
getLastMonoWave psql = do
        ans <- PSQL.query_ psql
                ("SELECT * "
                        <> " FROM public.monowaves "
                        <> " ORDER BY id DESC "
                        <> " LIMIT 1 "
                        <> ";"
                )
        case ans of
                [] -> return Nothing
                (row:_) -> return $ Just row

newMonoWave :: PSQL.Connection
            -> Integer
            -> (Time.UTCTime, Time.UTCTime)
            -> (Rational, Rational)
            -> Int
            -> Rational
            -> Rational
            -> Rational
            -> Rational
            -> Rational
            -> IO MonoWaveId
newMonoWave psql order (t1,t2) (p1,p2) count volume amount pAvg pVwavg pAwavg = do
        [(PSQL.Only mwId)] <- PSQL.query psql
                (" INSERT INTO public.monowaves "
                        <> " ( "
                                <> "   id "
                                <> " , time_range "
                                <> " , up "
                                <> " , px_range "
                                <> " , trades_count "
                                <> " , volume "
                                <> " , amount "
                                <> " , px_avg "
                                <> " , px_vwavg "
                                <> " , px_awavg "
                        <> " ) "
                        <> " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
                        <> " RETURNING id "
                        <> " ; "
                )
                (
                          order
                        , if t1 == t2
                                -- Without this a time range of (t1,t1] is
                                -- stored as 'empty';
                                then PGR.PGRange
                                        (PGR.Inclusive t1)
                                        (PGR.Inclusive t2)
                                else PGR.PGRange
                                        (PGR.Exclusive t1)
                                        (PGR.Inclusive t2)
                        , (if p2 > p1 then True else False)
                        , (if p2 > p1
                                then PGR.PGRange
                                        (PGR.Exclusive $ fuckingScientific p1)
                                        (PGR.Inclusive $ fuckingScientific p2)
                                else PGR.PGRange
                                        (PGR.Exclusive $ fuckingScientific p2)
                                        (PGR.Inclusive $ fuckingScientific p1)
                        )
                        , count
                        , (fuckingScientific volume)
                        , (fuckingScientific amount)
                        , (fuckingScientific pAvg)
                        , (fuckingScientific pVwavg)
                        , (fuckingScientific pAwavg)
                )
        return mwId

--------------------------------------------------------------------------------

-- MUST BE RUN INSIDE A TRANSACTION!
-- Fold function must return Nothing when
-- TODO: From postgresql-simple version 0.5.3.0 there's a natve cursor function.
-- But I can't find a way to stop this function, to close the cursor.
foldLimitOffset :: (Show a, PSQL.ToRow q, PSQL.FromRow a)
                => PSQL.Connection
                -> (Int,Int)
                -> PSQL.Query
                -- Query parameters.
                -> q
                -- The fold function.
                -> (b -> a -> (Either b b))
                -- The initial value of the fold.
                -> b
                -> IO b
foldLimitOffset psql (limit,offset) sql _ f b0 = do
        fans <- let
                cursor x l o = do
                        ans <- PSQL.query psql
                                (sql <> " LIMIT ? OFFSET ? ")
                                (l,o)
                        case ans of
                                ([]) -> return x
                                rows -> case (foldM f x rows) of
                                        -- Stop recursing.
                                        (Left x') -> return x'
                                        -- More rows needed.
                                        (Right x') -> cursor x' l (o + l)
                in cursor b0 limit offset
        return fans

--------------------------------------------------------------------------------

-- MUST BE RUN INSIDE A TRANSACTION!
getUnusedTrades :: (Show a) => PSQL.Connection
                -> (a -> BinanceDB.Trade -> (Either a a))
                -> a
                -> IO a
{--
getUnusedTrades psql f acc = do
        fans <- let
                cursor x l o = do
                        ----------
                        print ("Limit " ++ (show l) ++ " offset " ++ (show o))
                        ----------
                        ans <- PSQL.query psql
                                (" SELECT get_unmonowaved_trades(?,?); ")
                                (l,o)
                        ----------
                        print "ANS"
                        ----------
                        case ans of
                                ([]) -> return x
                                rows -> case (batch x rows) of
                                        -- Stop recursing.
                                        (Left x') -> return x'
                                        -- More rows needed.
                                        (Right x') -> cursor x' l (o + l)
                batch = foldM
                        (\x r -> case (f x r) of
                                -- Function says stop recursing.
                                -- Return previous value.
                                Nothing -> (Left x)
                                -- Function needs more values.
                                (Just x') -> (Right x')
                        )
                in cursor acc (15::Integer) (0::Integer)
        return fans
--}
getUnusedTrades psql f acc = foldLimitOffset psql (15, 0)
        ("SELECT * "
                <> " FROM public.trades AS trades "
                <> " WHERE NOT EXISTS ( "
                        <> " SELECT 1 "
                        <> " FROM public.monowaves_trades AS monowave_trades "
                        <> " WHERE monowave_trades.trades_id = trades.trade_id "
                <> " ) "
                <> " ORDER BY trades.trade_id ASC "
        )
        ()
        f
        acc
{--
getUnusedTrades psql f acc = foldCursor psql
        ("SELECT * "
                <> " FROM public.trades AS trades "
                <> " WHERE NOT EXISTS ( "
                        <> " SELECT 1 "
                        <> " FROM public.monowaves_trades AS monowave_trades "
                        <> " WHERE monowave_trades.trades_id = trades.id "
                <> " ) "
                <> " ORDER BY trades.trade_id ASC "
        )
        ()
        f
        acc
--}

{--

http://tatiyants.com/pev/#/plans/new


Option 1)

EXPLAIN SELECT trades.*
FROM public.trades AS trades
LEFT JOIN public.monowaves_trades AS monowaves_trades
ON trades.id = monowaves_trades.trades_id
WHERE monowaves_trades.id IS NULL
ORDER BY trades.trade_id ASC LIMIT 50 OFFSET 0;
                                                  QUERY PLAN                                                   
---------------------------------------------------------------------------------------------------------------
 Limit  (cost=4877216.59..4877216.59 rows=1 width=55)
   ->  Sort  (cost=4877216.59..4877216.59 rows=1 width=55)
         Sort Key: trades.trade_id
         ->  Gather  (cost=4212.87..4877216.58 rows=1 width=55)
               Workers Planned: 2
               ->  Parallel Hash Left Join  (cost=3212.87..4876216.48 rows=1 width=55)
                     Hash Cond: (trades.id = monowaves_trades.trades_id)
                     Filter: (monowaves_trades.id IS NULL)
                     ->  Parallel Seq Scan on trades  (cost=0.00..4554872.40 rows=121192840 width=55)
                     ->  Parallel Hash  (cost=2065.72..2065.72 rows=91772 width=16)
                           ->  Parallel Seq Scan on monowaves_trades  (cost=0.00..2065.72 rows=91772 width=16)
 JIT:
   Functions: 14
   Options: Inlining true, Optimization true, Expressions true, Deforming true
(14 rows)

Option 2)

EXPLAIN SELECT *
FROM public.trades          
WHERE id NOT IN (SELECT trades_id FROM public.monowaves_trades)
ORDER BY trade_id ASC LIMIT 50 OFFSET 0;
                                                        QUERY PLAN                                                        
--------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=1000.60..95675.19 rows=50 width=55)
   ->  Gather Merge  (cost=1000.60..275373203949.47 rows=145431408 width=55)
         Workers Planned: 2
         ->  Parallel Index Scan using trades_trade_id_key on trades  (cost=0.57..275356416556.45 rows=60596420 width=55)
               Filter: (NOT (SubPlan 1))
               SubPlan 1
                 ->  Materialize  (cost=0.00..4137.60 rows=157507 width=8)
                       ->  Seq Scan on monowaves_trades  (cost=0.00..2734.07 rows=157507 width=8)
(8 rows)

Option 3) THIS ONE!!!!

EXPLAIN SELECT *
FROM public.trades AS trades
WHERE NOT EXISTS (
        SELECT 1 FROM public.monowaves_trades AS monowave_trades
        WHERE monowave_trades.trades_id = trades.id
)
ORDER BY trade_id ASC LIMIT 50 OFFSET 0;
                                                               QUERY PLAN                                                               
----------------------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=0.99..492.94 rows=50 width=55)
   ->  Nested Loop Anti Join  (cost=0.99..2860217418.72 rows=290703950 width=55)
         ->  Index Scan using trades_trade_id_key on trades  (cost=0.57..772890192.06 rows=290862816 width=55)
         ->  Index Only Scan using monowaves_trades_trades_id_key on monowaves_trades monowave_trades  (cost=0.42..7.18 rows=1 width=8)
               Index Cond: (trades_id = trades.id)
(5 rows)

--}

--------------------------------------------------------------------------------

newMonoWaveTrade :: PSQL.Connection
                 -> BinanceDB.TradeId
                 -> MonoWaveId
                 -> IO ()
newMonoWaveTrade psql tradeId mwId = do
        _ <- PSQL.execute psql
                (" INSERT INTO public.monowaves_trades "
                        <> " ( "
                                <> "   trades_id "
                                <> " , monowaves_id "
                        <> " ) "
                        <> " VALUES (?,?) "
                        <> " ; "
                )
                (tradeId, mwId)
        return ()
