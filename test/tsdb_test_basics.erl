-module(tsdb_test_basics).

-export([test/0]).

-define(basic_tsdb, "/tmp/tsdb-test-basics.tsdb").

test() ->
    delete_db(?basic_tsdb),
    {ok, Db} = tsdb_db:start_link(),
    ok = tsdb_db:open(Db, ?basic_tsdb),
    xxx.
    
