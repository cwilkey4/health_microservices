-module(http_server).
-export([start/0, init/3, handle_request/2, terminate/3]).

%% Start Cowboy HTTP server
start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/anonymize", ?MODULE, []}
        ]}
    ]),
    cowboy:start_clear(my_http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }).

%% Initialize the HTTP request handler
init(_, Req, _) ->
    {cowboy_rest, Req, undefined}.

%% Handle incoming anonymization requests
handle_request(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body), % Parse JSON input
    AnonymizedData = anonymizer:pseudonymize_name(maps:get(<<"name">>, Data)),
    Response = jsx:encode(#{<<"anonymized_name">> => AnonymizedData}),
    {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1),
    {stop, Req2, State}.

%% Cleanup function (Required by Cowboy, but no special cleanup needed here)
terminate(_Reason, _Req, _State) ->
    ok.