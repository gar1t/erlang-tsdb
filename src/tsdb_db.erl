%% Copyright (C) 2012, 2013 Garrett Smith <g@rre.tt>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(tsdb_db).

-behaviour(gen_server).

-export([start_link/0,
         ping/1,
         ping/2,
         open/2,
         open/3,
         close/1,
         flush/1,
         info/1,
         goto_epoch/2,
         set_values/3,
         get_values/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

-define(DEFAULT_PING_TIMEOUT, 1000).
-define(DEFAULT_SLOT_SECONDS, 60).
-define(DEFAULT_VALUES_PER_ENTRY, 1).

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

ping(Db) -> ping(Db, ?DEFAULT_PING_TIMEOUT).

ping(Db, Timeout) ->
    try
        gen_server:call(Db, ping, Timeout)
    catch
        error:timeout -> timeout
    end.

close(Db) ->
    gen_server:call(Db, close, infinity).

flush(Db) ->
    gen_server:call(Db, flush, infinity).

open(Db, File) ->
    open(Db, File, []).

%%--------------------------------------------------------------------
%% @doc Opens a database.
%%
%% Default value for slot_seconds is 60. Default for values_per_entry
%% is 1. read_only defaults to false.
%%
%% @spec open(Db, File, Options) -> ok | {error, Reason}
%% Db = pid() | atom()
%% File = string()
%% Options = [option()]
%% option() = {slot_seconds, integer()}
%%          | {values_per_entry, integer()}
%%          | read_only
%% Readon = already_opened | {tsdb_open, integer()}
%% @end
%%--------------------------------------------------------------------

open(Db, File, Options) ->
    SlotSeconds = slot_seconds_option(Options),
    ValuesPerEntry = values_per_entry_option(Options),
    ReadOnly = read_only_option(Options),
    Open = {open, File, SlotSeconds, ValuesPerEntry, ReadOnly},
    gen_server:call(Db, Open, infinity).

slot_seconds_option(Options) ->
    validate_slot_seconds(
      proplists:get_value(slot_seconds, Options, ?DEFAULT_SLOT_SECONDS)).

validate_slot_seconds(I) when is_integer(I), I > 0 -> I;
validate_slot_seconds(Other) -> error({invalid_slot_seconds, Other}).

values_per_entry_option(Options) ->
    validate_values_per_entry(
      proplists:get_value(
        values_per_entry, Options, ?DEFAULT_VALUES_PER_ENTRY)).

validate_values_per_entry(I) when is_integer(I), I > 0 -> I;
validate_values_per_entry(Other) -> error({invalid_values_per_entry, Other}).

read_only_option(Options) ->
    bool_to_int(proplists:get_bool(read_only, Options)).

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.

info(Db) ->
    gen_server:call(Db, info, infinity).

%%--------------------------------------------------------------------
%% @doc Sets the database to read/write at a particular epoch.
%%
%% @spec goto_epoch(Db, Epoch) -> ok | {error, Reason}
%% Db = pid() | atom()
%% Epoch = integer()
%% Reason = not_open | {tsdb_goto_epoch, integer()}
%% @end
%%--------------------------------------------------------------------

goto_epoch(Db, Epoch) when is_integer(Epoch), Epoch >= 0 ->
    gen_server:call(Db, {goto, Epoch}, infinity).

%%--------------------------------------------------------------------
%% @doc Sets values for a key at the current epoch.
%%
%% @spec set_values(Db, Key, Values) -> ok | {error, Reason}
%% Db = pid() | atom()
%% Key = string()
%% Values = [integer()]
%% Reason = not_open | missing_epoch | too_many_values
%%        | {tsdb_set, integer()}
%% @end
%%--------------------------------------------------------------------

set_values(Db, Key, Values) ->
    gen_server:call(Db, {set, Key, Values}, infinity).

%%--------------------------------------------------------------------
%% @doc Gets values for a key at the current epoch.
%%
%% @spec get_values(Db, Key) -> {ok, Values} | {error, Reason}
%% Db = pid() | atom()
%% Key = string()
%% Values = [integer()]
%% Reason = not_found | not_open | missing_epoch
%%        | {tsdb_get, integer()}
%%--------------------------------------------------------------------

get_values(Db, Key) ->
    gen_server:call(Db, {get, Key}, infinity).

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

handle_info({Port, {exit_status, Exit}}, #state{port=Port}=State) ->
    {stop, {port_process_exit, Exit}, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
    {stop, {port_exit, Reason}, State};
handle_info(Msg, State) ->
    {stop, {unhandled_msg, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
