-module(tsdb_test_basics).

-export([test/0]).

-define(basic_tsdb, "/tmp/tsdb-test-basics.tsdb").

test() ->
    delete_db(?basic_tsdb),
    {ok, Db} = tsdb_db:start_link(),
    ok = tsdb_db:open(Db, ?basic_tsdb),

    Days = 1,
    ValueCount = 200000,

    From = tsdb_epoch:utc_ago({days, Days}),
    To = tsdb_epoch:utc_now(),
    Values = [{value_key(I), I} || I <- lists:seq(1, ValueCount)],

    status("Setting values from ~b to ~b: ", [From, To]),
    {SetTime, ok} = timer:tc(fun set_values/4, [Db, From, To, Values]),
    status(
      "~nTook ~b millis (~b values per second)~n",
      [SetTime div 1000,
       values_per_second({days, Days}, ValueCount, SetTime)]),

    status("Reading values back: "),
    {GetTime, ok} = timer:tc(fun get_values/4, [Db, From, To, Values]),
    status(
      "~nTook ~b millis (~b values per second)~n",
      [GetTime div 1000,
       values_per_second({days, Days}, ValueCount, GetTime)]),

    ok = tsdb_db:close(Db).

delete_db(File) ->
    file:delete(File).

value_key(I) ->
    io_lib:format("key-~b", [I]).

status(Msg) -> io:format(Msg).

status(Msg, Args) -> io:format(Msg, Args).

set_values(_Db, Cur, End, _Values) when Cur > End -> ok;
set_values(Db, Cur, End, Values) ->
    ok = tsdb_db:goto_epoch(Db, Cur),
    set_values(Db, Values),
    set_values(Db, Cur + 60, End, Values).

set_values(_Db, []) -> ok;
set_values(Db, [{Key, Value}|Rest]) ->
    ok = tsdb_db:set_values(Db, Key, [Value]),
    set_values(Db, Rest).

get_values(_Db, Cur, End, _Values) when Cur > End -> ok;
get_values(Db, Cur, End, Values) ->
    ok = tsdb_db:goto_epoch(Db, Cur),
    get_values(Db, Values),
    get_values(Db, Cur + 60, End, Values).

get_values(_Db, []) -> ok;
get_values(Db, [{Key, Value}|Rest]) ->
    {ok, [Value]} = tsdb_db:get_values(Db, Key),
    get_values(Db, Rest).

values_per_second({days, Days}, Values, TimeMicro) ->
    Count = Days * 24 * 60 * Values,
    trunc(Count / (TimeMicro / 1000000)).
