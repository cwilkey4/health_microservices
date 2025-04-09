%% =========== %%
%% Unit Tests  %%
%% =========== %%

-module(appt_test).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
    appointments = []}).

% Setup and Teardown
setup() ->
    ok.

teardown(_) ->
    ok.

% Test Suite
handle_cast_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun schedule_happy_path_/0,
            fun schedule_patient_conflict_/0,
            fun schedule_time_conflict_/0,
            fun reschedule_happy_path_/0,
            fun reschedule_time_conflict_/0,
            fun reschedule_no_existing_appt_/0
        ]
    }.

% Helper function to create a state record
create_state(Appointments) ->
    #state{appointments = Appointments}.

% schedule/2 Tests

schedule_happy_path_() ->
    State = create_state([]),
    NewTime = {2025, 4, 10, 9, 0, 0},
    {noreply, NewState} = appt:handle_cast({schedule, patient_1, NewTime}, State),
    ?assert(lists:member({patient_1, NewTime, scheduled}, NewState#state.appointments)).

schedule_patient_conflict_() ->
    ExistingTime = {2025, 4, 10, 9, 0, 0},
    State = create_state([{patient_1, ExistingTime, scheduled}]),
    {noreply, NewState} = appt:handle_cast({schedule, patient_1, ExistingTime}, State),
    ?assertEqual(State, NewState).

schedule_time_conflict_() ->
    ExistingTime = {2025, 4, 10, 9, 0, 0},
    State = create_state([{patient_2, ExistingTime, scheduled}]),
    {noreply, NewState} = appt:handle_cast({schedule, patient_1, ExistingTime}, State),
    ?assertEqual(State, NewState).

% reschedule/2 Tests

reschedule_happy_path_() ->
    ExistingTime = {2025, 4, 10, 9, 0, 0},
    NewTime = {2025, 4, 11, 10, 0, 0},
    State = create_state([{patient_1, ExistingTime, scheduled}]),
    {noreply, NewState} = appt:handle_cast({reschedule, patient_1, NewTime}, State),
    ?assertEqual([{patient_1, NewTime, scheduled}], NewState#state.appointments).

reschedule_time_conflict_() ->
    _ExistingTime = {2025, 4, 10, 9, 0, 0},
    NewTime = {2025, 4, 11, 10, 0, 0},
    State = create_state([{patient_2, NewTime, scheduled}]),
    {noreply, NewState} = appt:handle_cast({reschedule, patient_1, NewTime}, State),
    ?assertEqual(State, NewState).

reschedule_no_existing_appt_() ->
    State = create_state([]),
    NewTime = {2025, 4, 11, 10, 0, 0},
    {noreply, NewState} = appt:handle_cast({reschedule, patient_1, NewTime}, State),
    ?assertEqual(State, NewState).
