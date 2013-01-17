-module(tsdb_db).

-behaviour(gen_server).

-export([start_link/0,
         ping/2,
         open/2,
         open/3,
         close/1,
         info/1,
         goto_epoch/2,
         goto_epoch/3,
         set_values/3,
         get_values/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Port = start_port(),
    {ok, #state{port=Port}}.

start_port() ->
    open_port({spawn, tsdb_port_exe()}, [{packet, 2}, binary, exit_status]).

tsdb_port_exe() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    filename:join([EbinDir, "..", "priv", "tsdb-port"]).

%%%===================================================================
%%% API
%%%===================================================================

ping(Db, Timeout) ->
    try
        gen_server:call(Db, ping, Timeout)
    catch
        error:timeout -> timeout
    end.

open(Db, File) ->
    open(Db, File, []).

close(Db) ->
    gen_server:call(Db, close, infinity).

open(Db, File, Options) ->
    gen_server:call(Db, {open, File, Options}, infinity).

info(Db) ->
    gen_server:call(Db, info, infinity).

goto_epoch(Db, Epoch) ->
    goto_epoch(Db, Epoch, []).

goto_epoch(Db, Epoch, Options) ->
    gen_server:call(Db, {goto_epoch, Epoch, Options}, infinity).

set_values(Db, Key, Values) ->
    gen_server:call(Db, {set_values, Key, Values}, infinity).

get_values(Db, Key) ->
    gen_server:call(Db, {get_values, Key}, infinity).

%%%===================================================================
%%% Callbacks
%%%===================================================================

handle_call(Msg, _From, #state{port=Port}=State) ->
    erlang:send(Port, {self(), {command, term_to_binary(Msg)}}),
    receive
        {Port, {data, Data}} ->
            {reply, binary_to_term(Data), State};
        {Port, {exit_status, Status}} ->
            exit({port_exit, Status});
        {'EXIT', Port, Reason} ->
            exit({port_exit, Reason})
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
