%% =========================== %%
%% Appointment Service Module  %%
%% @author Celestia Wilkey     %%
%% @version 1.0                %%
%% @since 2025-04-09           %%
%% =========================== %%

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

%% @doc
%% Handles asynchronous cast messages for scheduling, rescheduling, cancelling,
%% and completing patient appointments. Each operation updates the state based on
%% constraints such as existing patient schedules and time slot conflicts.
%%
%% === Supported Cast Messages ===
%% 
%% <ul>
%%   <li>`{schedule, PatientId, Time}` – Schedules a new appointment if the patient
%%        doesn't already have one and the time slot is available.</li>
%%   <li>`{reschedule, PatientId, Time}` – Reschedules an existing appointment for
%%        a patient if the time slot is free.</li>
%%   <li>`{cancel, PatientId}` – Cancels a patient's scheduled appointment.</li>
%%   <li>`{complete, PatientId}` – Marks a patient's appointment as completed.</li>
%% </ul>
%%
%% === Schedule Clause ===
%% Checks for patient availability and time slot conflicts before adding an appointment.
%% Logs warnings if constraints are violated.
%%
%% === Reschedule Clause ===
%% Validates time slot availability and ensures the patient has a scheduled appointment
%% before updating it.
%%
%% === Cancel Clause ===
%% Removes the patient’s appointment and logs the cancellation.
%%
%% === Complete Clause ===
%% Marks the patient’s scheduled appointment as completed in the state.
%%
%% @spec handle_cast(Msg :: tuple(), State :: #state{}) ->
%%           {noreply, #state{}} when
%%           Msg :: {schedule, PatientId :: term(), Time :: calendar:datetime()} |
%%                   {reschedule, PatientId :: term(), Time :: calendar:datetime()} |
%%                   {cancel, PatientId :: term()} |
%%                   {complete, PatientId :: term()}.
%%
%% @complexity
%% - `schedule`: O(N), where N is the number of existing appointments (list scan).
%% - `reschedule`: O(N), due to scans and a replacement operation.
%% - `cancel`: O(N), due to removal logic in `remove_appt/2`.
%% - `complete`: O(N), due to modification via `complete_appt/2`.
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


%% @doc
%% Handles synchronous call messages related to appointment state management.
%%
%% === Supported Call Messages ===
%% 
%% <ul>
%%   <li>`stop` – Stops the server gracefully, returning `ok` as the reply.</li>
%%   <li>`reset` – Resets the state to an empty appointment list.</li>
%%   <li>`{find_appt, PatientId}` – Returns a list of appointments matching the given `PatientId`.</li>
%%   <li>`get_state` – Returns the current state.</li>
%% </ul>
%%
%% === Stop Clause ===
%% Gracefully stops the server and returns `ok`.
%%
%% === Reset Clause ===
%% Resets the internal state, clearing all scheduled appointments.
%%
%% === Find Appointment Clause ===
%% Retrieves appointments for the specified `PatientId` from the state.
%%
%% === Get State Clause ===
%% Returns the entire current state.
%%
%% @spec handle_call(Msg :: term(), From :: term(), State :: #state{}) ->
%%           {reply, Reply :: term(), #state{}} |
%%           {stop, Reason :: term(), Reply :: term(), #state{}} when
%%           Msg :: stop |
%%                   reset |
%%                   {find_appt, PatientId :: term()} |
%%                   get_state.
%%
%% @complexity
%% - `stop`: O(1), constant-time operation.
%% - `reset`: O(1), resets the state structure.
%% - `find_appt`: O(N), where N is the number of appointments.
%% - `get_state`: O(1), returns the state as-is.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(reset, _From, _State) ->
    {reply, reset, #state{appointments = []}};

handle_call({find_appt, PatientId}, _From, State) ->
    Result = lists:filter(fun({Id, _, _}) -> Id =:= PatientId end, State#state.appointments),
    {reply, Result, State};

handle_call(get_state, _From, State) ->
    {reply, State, State}.

terminate(_Reason, _State) ->
    ok.


%% HELPER FUNCTIONS %%

%% @doc
%% Replaces the scheduled time for a patient's existing appointment.
%% If the patient is found, their appointment time is updated to the new time,
%% and their status is set to `scheduled`.
%%
%% @spec replace_time(PatientId :: term(), Time :: calendar:datetime(), Appts :: list()) -> list().
%%
%% @complexity O(N), where N is the length of the appointment list. The function
%% traverses the list until the first matching appointment is found.

replace_time(_, _, []) -> [];
replace_time(PatientId, Time, [{PatientId, _, _} | Rest]) ->
    [{PatientId, Time, scheduled} | Rest];
replace_time(PatientId, Time, [H | Rest]) ->
    [H | replace_time(PatientId, Time, Rest)].

%% @doc
%% Marks a patient's scheduled appointment as completed by changing the status
%% from `scheduled` to `complete`. Only the first matching appointment is updated.
%%
%% @spec complete_appt(PatientId :: term(), Appts :: list()) -> list().
%%
%% @complexity O(N), where N is the number of appointments. The function traverses
%% the list to locate and update the matching appointment.

complete_appt(_, []) -> [];
complete_appt(PatientId, [{PatientId, Time, scheduled} | Rest]) ->
    [{PatientId, Time, complete} | Rest];
complete_appt(PatientId, [H | Rest]) ->
    [H | complete_appt(PatientId, Rest)].

%% @doc
%% Removes a patient's appointment from the appointment list. Returns a tuple
%% containing the removed appointment's time and the updated appointment list.
%%
%% If the patient has no appointment, the returned time is `undefined`.
%%
%% @spec remove_appt(PatientId :: term(), Appts :: list()) -> {calendar:datetime() | undefined, list()}.
%%
%% @complexity O(N), where N is the number of appointments. The function must scan
%% through the list to find and remove the matching appointment.

remove_appt(_, []) -> {undefined, []};
remove_appt(PatientId, [{PatientId, Time, _} | Rest]) ->
    {Time, Rest};
remove_appt(PatientId, [H | Rest]) ->
    {Time, Tail} = remove_appt(PatientId, Rest),
    {Time, [H | Tail]}.
