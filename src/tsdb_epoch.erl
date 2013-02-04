-module(tsdb_epoch).

-export([utc_now/0, utc_ago/1, local_now/0, local_ago/1]).

-define(EPOCH_SECONDS, 62167219200).

utc_now() ->
    epoch_seconds(calendar:universal_time()).

epoch_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH_SECONDS.

utc_ago(Interval) ->
    utc_now() - interval_seconds(Interval).

interval_seconds(N) when is_integer(N) -> N;
interval_seconds({seconds, N}) -> N;
interval_seconds({minutes, N}) -> N * 60;
interval_seconds({hours, N}) -> N * 3600;
interval_seconds({days, N}) -> N * 86400.

local_now() ->
    epoch_seconds(calendar:local_time()).

local_ago(Interval) ->
    local_now() - interval_seconds(Interval).
