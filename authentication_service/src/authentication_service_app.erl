%%%-------------------------------------------------------------------
%% @doc authentication_service public API
%% @end
%%%-------------------------------------------------------------------

-module(authentication_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    authentication_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
