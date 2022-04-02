CREATE DATABASE binance
        OWNER postgres
        TEMPLATE DEFAULT
        ENCODING UTF8
        LC_COLLATE 'en_US.UTF-8'
        LC_CTYPE 'en_US.UTF-8'
        TABLESPACE DEFAULT
        ALLOW_CONNECTIONS TRUE
        CONNECTION LIMIT -1
        IS_TEMPLATE FALSE
;

CREATE ROLE fmaste WITH
        NOSUPERUSER
        NOCREATEDB
        NOCREATEROLE
        NOINHERIT
        LOGIN
        NOREPLICATION
        NOBYPASSRLS
        CONNECTION LIMIT -1
        -- ENCRYPTED
        PASSWORD NULL
        -- VALID UNTIL 'timestamp'
        -- IN ROLE role_name [, ...]
        -- IN GROUP role_name [, ...]
        -- ROLE role_name [, ...]
        -- ADMIN role_name [, ...]
        -- USER role_name [, ...]
        -- SYSID uid
;

GRANT ALL PRIVILEGES ON SCHEMA public TO fmaste;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public TO fmaste;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO fmaste;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO fmaste;

--------------------------------------------------------------------------------

SET search_path TO public;

--------------------------------------------------------------------------------

CREATE EXTENSION btree_gist;

--------------------------------------------------------------------------------

CREATE TABLE public.trades (
        -- As provided by Binance, asumming it is a primary key.
        trade_id BIGINT NOT NULL,
-- Individual data: ************************************************************
--******************************************************************************
        trade_px DECIMAL NOT NULL,
        trade_qt DECIMAL NOT NULL,
        trade_quote_qt DECIMAL NOT NULL,
        -- Can be repeated.
        trade_time TIMESTAMP WITH TIME ZONE NOT NULL,
        trade_buyer_maker BOOLEAN NOT NULL,
        trade_best_match BOOLEAN NOT NULL,
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(trade_id) NOT DEFERRABLE
) WITHOUT OIDS;

CREATE INDEX trades_ix_btree_time ON public.trades (
        trade_time ASC
);

CREATE INDEX trades_ix_hash_id ON public.trades USING HASH (
        trade_id
);

--------------------------------------------------------------------------------

CREATE TABLE public.monowaves (
        id BIGINT NOT NULL,
-- Individual data: ************************************************************
--******************************************************************************
        -- Range of timestamp with time zone.
        time_range tstzrange NOT NULL,
        -- Wave is going up ?
        up BOOLEAN NOT NULL,
        -- Range of prices. Always from low to high.
        px_range numrange NOT NULL,
        -- How many trades.
        trades_count INT NOT NULL,
        -- Sum of quantities.
        volume DECIMAL NOT NULL,
        -- Sum of quotes qt.
        amount DECIMAL NOT NULL,
        -- average px.
        px_avg DECIMAL NOT NULL,
        -- Volume Weighted average px.
        px_vwavg DECIMAL NOT NULL,
        -- Amount Weighted average px.
        px_awavg DECIMAL NOT NULL,
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(id) NOT DEFERRABLE
) WITHOUT OIDS;

CREATE INDEX monowaves_ix_gist_time ON public.monowaves USING GIST (
        time_range
);

CREATE INDEX monowaves_ix_gist_price ON public.monowaves USING GIST (
        px_range
);

-- From the first trade that changed price direction.
-- To the last trade in this price direction.
CREATE TABLE public.monowaves_trades (
-- References: *****************************************************************
--******************************************************************************
        -- The trade.
        trades_id BIGINT NOT NULL
                REFERENCES public.trades (trade_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- The monowave.
        monowaves_id BIGINT NOT NULL
                REFERENCES public.monowaves (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(trades_id) NOT DEFERRABLE
) WITHOUT OIDS;

CREATE INDEX monowaves_trades_ix_hash_trades_id ON public.monowaves_trades USING HASH (
        trades_id
);

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION public.get_unmonowaved_trades (
          lim BIGINT, off BIGINT
) RETURNS SETOF public.trades AS $$
BEGIN
        RETURN QUERY
                SELECT *
                FROM public.trades AS trades
                WHERE NOT EXISTS (
                        SELECT 1 FROM public.monowaves_trades AS monowave_trades
                        WHERE monowave_trades.trades_id = trades.id
                ) LIMIT lim OFFSET off;
END;
$$ LANGUAGE plpgsql
        VOLATILE
        -- Option LEAKPROOF can only be set by the superuser.
        -- Can't use it with Amazon RDS.
        -- Functions which do not take arguments or which are not passed any
        -- arguments from the security barrier view or table do not have to be
        -- marked as leakproof to be executed before security conditions.
        -- LEAKPROOF
        SECURITY DEFINER
        -- Allowed to run in parallel so clock_timestamp() can return the same
        -- value. The value of seq_id will avoid an ID clash. Without running in
        -- parallel the 7 bits of the seq_id are useless.
        PARALLEL SAFE
        SET search_path = public
;


-- Candles as materialized views.
--------------------------------------------------------------------------------

CREATE MATERIALIZED VIEW public.trades_candles_1s AS
SELECT
        EXTRACT(YEAR from trade_time) AS year,
        EXTRACT(MONTH from trade_time) AS month,
        EXTRACT(DAY from trade_time) AS day,
        EXTRACT(HOUR from trade_time) AS hour,
        EXTRACT(MINUTE from trade_time) AS minute,
        TRUNC(EXTRACT(SECOND from trade_time)) AS second,
        -- Orders.
        COUNT(*) AS count,
        -- Volume.
        SUM(trade_qt) AS volume,
        -- Amount.
        SUM(trade_quote_qt) AS amount,
        -- Open.
        (array_agg(trade_px ORDER BY id ASC))[1] AS px_open,
        -- Min and max prices.
        MIN(trade_px) AS px_min,
        MAX(trade_px) AS px_max,
        -- Close.
        (array_agg(trade_px ORDER BY id DESC))[1] AS px_close,
        -- Average px.
        AVG(trade_px) AS px_avg,
        -- Volume Weighted average px.
        SUM(trade_px*trade_qt)/SUM(trade_qt) AS px_vwavg
FROM
        public.trades
GROUP BY
        EXTRACT(YEAR from trade_time),
        EXTRACT(MONTH from trade_time),
        EXTRACT(DAY from trade_time),
        EXTRACT(HOUR from trade_time),
        EXTRACT(MINUTE from trade_time),
        TRUNC(EXTRACT(SECOND from trade_time))
ORDER BY
        EXTRACT(YEAR from trade_time) ASC,
        EXTRACT(MONTH from trade_time) ASC,
        EXTRACT(DAY from trade_time) ASC,
        EXTRACT(HOUR from trade_time) ASC,
        EXTRACT(MINUTE from trade_time) ASC,
        TRUNC(EXTRACT(SECOND from trade_time)) ASC
;

CREATE MATERIALIZED VIEW public.trades_candles_1m AS
SELECT
        EXTRACT(YEAR from trade_time) AS year,
        EXTRACT(MONTH from trade_time) AS month,
        EXTRACT(DAY from trade_time) AS day,
        EXTRACT(HOUR from trade_time) AS hour,
        EXTRACT(MINUTE from trade_time) AS minute,
        -- Orders.
        COUNT(*) AS count,
        -- Volume.
        SUM(trade_qt) AS volume,
        -- Amount.
        SUM(trade_quote_qt) AS amount,
        -- Open.
        (array_agg(trade_px ORDER BY id ASC))[1] AS px_open,
        -- Min and max prices.
        MIN(trade_px) AS px_min,
        MAX(trade_px) AS px_max,
        -- Close.
        (array_agg(trade_px ORDER BY id DESC))[1] AS px_close,
        -- Average px.
        AVG(trade_px) AS px_avg,
        -- Volume Weighted average px.
        SUM(trade_px*trade_qt)/SUM(trade_qt) AS px_vwavg
FROM
        public.trades
GROUP BY
        EXTRACT(YEAR from trade_time),
        EXTRACT(MONTH from trade_time),
        EXTRACT(DAY from trade_time),
        EXTRACT(HOUR from trade_time),
        EXTRACT(MINUTE from trade_time)
ORDER BY
        EXTRACT(YEAR from trade_time) ASC,
        EXTRACT(MONTH from trade_time) ASC,
        EXTRACT(DAY from trade_time) ASC,
        EXTRACT(HOUR from trade_time) ASC,
        EXTRACT(MINUTE from trade_time) ASC
;

-- TODO:
-- ERROR:  functions in index expression must be marked IMMUTABLE
CREATE INDEX trades_ix_time_intervals ON public.trades (
        EXTRACT(YEAR from trade_time) ASC,
        EXTRACT(MONTH from trade_time) ASC,
        EXTRACT(DAY from trade_time) ASC,
        EXTRACT(HOUR from trade_time) ASC,
        EXTRACT(MINUTE from trade_time) ASC,
        TRUNC(EXTRACT(SECOND from trade_time)) ASC
);

CREATE MATERIALIZED VIEW public.trades_candles AS
SELECT
        EXTRACT(YEAR from trade_time) AS year,
        EXTRACT(MONTH from trade_time) AS month,
        EXTRACT(DAY from trade_time) AS day,
        EXTRACT(HOUR from trade_time) AS hour,
        EXTRACT(MINUTE from trade_time) AS minute,
        TRUNC(EXTRACT(SECOND from trade_time)) AS second,
        -- Orders.
        COUNT(*) AS count,
        -- Volume.
        SUM(trade_qt) AS volume,
        -- Amount.
        SUM(trade_quote_qt) AS amount,
        -- Open.
        (array_agg(trade_px ORDER BY id ASC))[1] AS px_open,
        -- Min and max prices.
        MIN(trade_px) AS px_min,
        MAX(trade_px) AS px_max,
        -- Close.
        (array_agg(trade_px ORDER BY id DESC))[1] AS px_close,
        -- Average px.
        AVG(trade_px) AS px_avg,
        -- Volume Weighted average px.
        SUM(trade_px*trade_qt)/SUM(trade_qt) AS px_vwavg
FROM
        public.trades
GROUP BY
        EXTRACT(YEAR from trade_time),
        EXTRACT(MONTH from trade_time),
        EXTRACT(DAY from trade_time),
        EXTRACT(HOUR from trade_time),
        EXTRACT(MINUTE from trade_time),
        TRUNC(EXTRACT(SECOND from trade_time))
ORDER BY
        EXTRACT(YEAR from trade_time) ASC,
        EXTRACT(MONTH from trade_time) ASC,
        EXTRACT(DAY from trade_time) ASC,
        EXTRACT(HOUR from trade_time) ASC,
        EXTRACT(MINUTE from trade_time) ASC,
        TRUNC(EXTRACT(SECOND from trade_time)) ASC
;

-- TODO:
CREATE INDEX trades_ix_year_stats ON public.trades (
        -- The quarter of the year (1 - 4) that the date is in.
        EXTRACT(QUARTER from trade_time) ASC,
        -- The number of the ISO 8601 week-numbering week of the year.
        -- By definition, ISO weeks start on Mondays and the first week of a
        -- year contains January 4 of that year. In other words, the first
        -- Thursday of a year is in week 1 of that year.
        EXTRACT(WEEK from trade_time) ASC,
        -- The ISO 8601 week-numbering year that the date falls in.
        EXTRACT(ISODOW from trade_time) ASC,
        -- The day of the year (1 - 365/366).
        EXTRACT(DOY from trade_time) ASC,
        COUNT(*),
        MIN(trade_px),
        MAX(trade_px),
        SUM(trade_qt)
);
