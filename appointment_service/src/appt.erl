-module(appt).
-behaviour(gen_server).

-export([
    start_link/0, stop/0, reset/0, schedule/1, reschedule/1, 
    cancel/1, complete/1, find_appt/1, get_state/0
]).
-export([
    init/1, handle_call/3, handle_cast/2, terminate/2
]).

-record(state, {
    id = doc_1,
    appointments = [] % {patient_id, time, status}
}).

%% API FUNCTIONS %%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

reset()->
    gen_server:call(?MODULE, reset).

schedule({PatientId, Time}) ->
    gen_server:cast(?MODULE, {schedule, PatientId, Time}).

reschedule({PatientId, Time}) ->
    gen_server:cast(?MODULE, {reschedule, PatientId, Time}).

cancel(PatientId) ->
    gen_server:cast(?MODULE, {cancel, PatientId}).

complete(PatientId) ->
    gen_server:cast(?MODULE, {complete, PatientId}).

find_appt(PatientId) ->
    gen_server:call(?MODULE, {find_appt, PatientId}).

get_state() ->
    gen_server:call(?MODULE, get_state).


%% GENSERVER CALLBACKS %%

init([]) ->
    {ok, #state{}}.

handle_cast({schedule, PatientId, Time}, State) ->
    Appts = State#state.appointments,

    PatientAvailability = lists:any(fun({Id, _ApptTime, Status}) ->
        Id =:= PatientId andalso Status =:= scheduled
    end, Appts),

    ApptAvailabilty = lists:any(fun({_Id, ApptTime, Status}) ->
        ApptTime =:= Time andalso Status =:= scheduled
    end, Appts),

    case {PatientAvailability, ApptAvailabilty} of
        {true, _} -> 
            logger:warning("Error: Patient ~p already has an appointment", [PatientId]),
            {noreply, State};
        {_, true} ->
            {Year, Month, Day, Hour, Minute, _} = Time,
            logger:warning("Error: Time slot (~p/~p/~p at ~p:~p) is already taken", [Month, Day, Year, Hour, Minute]),
            {noreply, State};
        _ ->
            NewAppt = {PatientId, Time, scheduled},
            NewState = State#state{appointments = [NewAppt|Appts]},
            {noreply, NewState}
    end;
        

handle_cast({reschedule, PatientId, Time}, State) ->
    Appts = State#state.appointments,

    PatientAvailability = lists:any(fun({Id, _ApptTime, Status}) ->
        Id =:= PatientId andalso Status =:= scheduled
    end, Appts),

    ApptAvailabilty = lists:any(fun({_Id, ApptTime, Status}) ->
        ApptTime =:= Time andalso Status =:= scheduled
    end, Appts),

    case {PatientAvailability, ApptAvailabilty} of
        {_, true} ->
            {Year, Month, Day, Hour, Minute, _} = Time,
            logger:warning("Error: Time slot (~p/~p/~p at ~p:~p) is already taken", [Month, Day, Year, Hour, Minute]),
            {noreply, State};
        {false, _} -> 
            logger:warning("Error: Patient ~p doesn't have an appointment scheduled", [PatientId]),
            {noreply, State};
        _ ->
            UpdatedAppts = replace_time(PatientId, Time, State#state.appointments),
            {noreply, State#state{appointments = UpdatedAppts}}
    end;


handle_cast({cancel, PatientId}, State) ->
    {Time, UpdatedAppts} = remove_appt(PatientId, State#state.appointments),
    logger:notice("Appointment cancelled for patient ~p on ~p", [PatientId, Time]),
    {noreply, State#state{appointments = UpdatedAppts}};

handle_cast({complete, PatientId}, State) ->
    UpdatedAppts = complete_appt(PatientId, State#state.appointments),
    {noreply, State#state{appointments = UpdatedAppts}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(reset, _From, _State) ->
    {reply, reset, #state{id = doc_1, appointments = []}};

handle_call({find_appt, PatientId}, _From, State) ->
    Result = lists:filter(fun({Id, _, _}) -> Id =:= PatientId end, State#state.appointments),
    {reply, Result, State};

handle_call(get_state, _From, State) ->
    {reply, State, State}.

terminate(_Reason, _State) ->
    ok.


%% HELPER FUNCTIONS %%

replace_time(_, _, []) -> [];
replace_time(PatientId, Time, [{PatientId, _, _} | Rest]) ->
    [{PatientId, Time, scheduled} | Rest];
replace_time(PatientId, Time, [H | Rest]) ->
    [H | replace_time(PatientId, Time, Rest)].

complete_appt(_, []) -> [];
complete_appt(PatientId, [{PatientId, Time, scheduled} | Rest]) ->
    [{PatientId, Time, complete} | Rest];
complete_appt(PatientId, [H | Rest]) ->
    [H | complete_appt(PatientId, Rest)].

remove_appt(_, []) -> {undefined, []};
remove_appt(PatientId, [{PatientId, Time, _} | Rest]) ->
    {Time, Rest};
remove_appt(PatientId, [H | Rest]) ->
    {Time, Tail} = remove_appt(PatientId, Rest),
    {Time, [H | Tail]}.