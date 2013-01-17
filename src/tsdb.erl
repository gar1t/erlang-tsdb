-module(tsdb).

-export([open_db/2, goto_epoch/2, goto_epoch/3, set_values/3, get_values/2]).

open_db(_File, _Options) ->
    xxx.

goto_epoch(Db, Epoch) ->
    goto_epoch(Db, Epoch, []).

goto_epoch(_Db, _Epoch, _Options) ->
    xxx.

set_values(_Db, _Key, _Values) ->
    xxx.

get_values(_Db, _Key) ->
    xxx.
