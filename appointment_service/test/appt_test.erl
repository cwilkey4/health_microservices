-module(appt_test).
-include_lib("eunit/include/eunit.hrl"). 

-record(state, {
    id = doc_1,
    appointments = []}).


%% Tests %% 
% Happy Path
appt_test() ->
    {
        setup,
        fun start_link/0,
        fun stop/0,
        fun(_) ->
            [
                fun test_schedule/0,
                fun test_reschedule/0,
                fun test_cancel/0,
                fun test_complete/0,
                fun test_find_appt/0
            ]
        end
    }.

%% Negative Paths %%
% Two schedules for one patient
% (Only one at a time is permitted -- the first will remain) 
two_schedules_test() ->
    {
        setup,
        fun start_link/0,
        fun reset/0,
        fun stop/0,
        fun two_schedules/0
    }.

% Two patients scheduling for the same time
% (The first appointment will remain)
overlaping_schedules_test() ->
    {
        setup,
        fun start_link/0,
        fun reset/0,
        fun stop/0,
        fun overlap_schedules/0
    }.

% Cancelling an appointment that doesn't exist.
% (State will remain the same)
cancel_test() ->
    {
        setup,
        fun start_link/0,
        fun reset/0,
        fun stop/0,
        fun cancel_appt/0
    }.


%% API %%
start_link() ->
    appt:start_link({local, appt}, appt, [], []).

stop() ->
    appt:stop(appt).

reset() ->
    appt:reset(appt).

%% Testers %%
test_schedule() ->
    ok = appt:schedule({1, {2025,6,16,10,0,0}}),
    ok = appt:schedule({2, {2025,6,16,11,0,0}}),
    ok = appt:schedule({3, {2025,6,16,13,0,0}}),

    timer:sleep(100),

    State = 
    #state{id = doc_1, 
        appointments = [{3,{2025,6,16,13,0,0}, scheduled},
                        {2,{2025,6,16,11,0,0}, scheduled},
                        {1,{2025,6,16,10,0,0}, scheduled}]},
    ?assertEqual(State, appt:get_state()).

test_reschedule() ->
    ok = appt:reschedule({1,{2025,6,17,10,0,0}}),

    timer:sleep(100),

    State =     
    #state{id = doc_1, 
        appointments = [{3,{2025,6,16,13,0,0}, scheduled},
                    {2,{2025,6,16,11,0,0}, scheduled},
                    {1,{2025,6,17,10,0,0}, scheduled}]},
    ?assertEqual(State, appt:get_state()).

test_cancel() ->
    ok = appt:cancel(2),

    timer:sleep(100),

    State = 
    #state{id = doc_1, 
    appointments = [{3,{2025,6,16,13,0,0}, scheduled},
                    {1,{2025,6,17,10,0,0}, scheduled}]},
    ?assertEqual(State, appt:get_state()).

test_complete() ->
    ok = appt:complete(3),

    timer:sleep(100),

    State = 
    #state{id = doc_1, 
    appointments = [{3,{2025,6,16,13,0,0}, complete},
                    {1,{2025,6,17,10,0,0}, scheduled}]},
    ?assertEqual(State, appt:get_state()).

test_find_appt() ->
    {1, {2025,6,17,10,0,0}} = appt:find_appt(1).


two_schedules() ->
    ok = appt:schedule({1, {2025,6,16,10,0,0}}),
    ok = appt:schedule({1, {2025,6,16,10,0,0}}),

    State = 
    #state{id = doc_1, 
        appointments = [{1,{2025,6,16,10,0,0}, scheduled}]},
    ?assertEqual(State, appt:get_state()).    

overlap_schedules() ->
    ok = appt:schedule({1, {2025,6,16,10,0,0}}),
    ok = appt:schedule({2, {2025,6,16,10,0,0}}),

    State = 
    #state{id = doc_1,
        appointments = [{1,{2025,6,16,10,0,0}}]},
    ?assertEqual(State, appt:get_state()).    
    
cancel_appt() ->
    ok = appt:cancel(1),
    State = #state{id = doc_1, appointments = []},

    ?assertEqual(State, appt:get_state()).