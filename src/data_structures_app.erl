%%%-------------------------------------------------------------------
%% @doc data_structures public API
%% @end
%%%-------------------------------------------------------------------

-module(data_structures_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    data_structures_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
