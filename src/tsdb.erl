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
