-module(data_handler).
-export([init/0, store_record/2, get_record/1]).

%% Initialize ETS table for storing anonymized data
init() ->
    ets:new(anonymized_data, [named_table, set, public, {keypos, 1}]).

%% Store anonymized record in ETS
store_record(ID, AnonymizedData) ->
    ets:insert(anonymized_data, {ID, AnonymizedData}).

%% Retrieve anonymized record by ID
get_record(ID) ->
    case ets:lookup(anonymized_data, ID) of
        [{_, Data}] -> {ok, Data};
        [] -> {error, not_found}
    end.

