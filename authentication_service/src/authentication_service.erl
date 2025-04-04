%%% ========================================================================================= %%%
%%%                            Authentication Service Module                                  %%%
%%% @author Jared Nash                                                                        %%% 
%%% @version 1.0                                                                              %%%  
%%% @since 2025-04-03                                                                         %%%  
%%% ========================================================================================= %%%

%% @doc Provides authentication functionalities using a Trie-based username store.
%% This module implements user registration, login, and password verification 
%% with a simple Trie data structure and a user store.
%%
%% @author Jared Nash
%% @version 1.0
%% @since 2025-04-03
%% @see add/2, contains/2, register_user/3, login_user/3, hash_password/1, verify_password/3
-module(authentication_service).
-export([new/0, add/2, contains/2, register_user/3, login_user/3, hash_password/1, verify_password/3]).

%% @doc Initializes an empty Trie and User Store.
%% The returned tuple consists of an empty Trie and an empty Users map.
%%
%% @return A tuple `{Trie, Users}`, where both are empty maps.
%% @since 2025-04-03
%% @complexity O(1)
new() -> {#{}, #{}}.  % {Trie, Users}

%% @doc Adds a username (as a list of characters) to the Trie.
%% This function recursively inserts characters from the given list
%% into the Trie, marking the end of the word when the list is empty.
%%
%% @param Username A list of characters representing the username.
%% @param Trie The current trie represented as a map.
%% @return A new trie map with the username inserted.
%% @since 2025-04-03
%% @complexity O(n), where n is the length of the username.
-spec add([char()], map()) -> map().
add([], Trie) -> Trie#{"end" => true};
add([H | T], Trie) ->
    SubTrie = maps:get(H, Trie, #{}),
    Trie#{H => add(T, SubTrie)}.

%% @doc Traverses the Trie to check if a username exists.
%% Returns `true` if the "end" marker is found.
%%
%% @param Username A list of characters representing the username.
%% @param Trie The current trie.
%% @return `true` if the username exists, `false` otherwise.
%% @since 2025-04-03
%% @complexity O(n), where n is the length of the username.
-spec contains([integer()], map()) -> boolean().
contains([], Trie) -> maps:is_key("end", Trie);
contains([H | T], Trie) ->
    case maps:find(H, Trie) of
        {ok, SubTrie} -> contains(T, SubTrie);
        error -> false
    end.

%% @doc Registers a new user with a hashed password.
%% This function first checks if the username already exists in the Trie.
%% If not, it adds the username to the Trie and stores the hashed password.
%%
%% @param Username The username as a binary string.
%% @param Password The user's password as a binary string.
%% @param AuthData A tuple `{Trie, Users}` representing the current authentication state.
%% @return `{ok, {NewTrie, NewUsers}}` if successful, or `{error, "User already exists"}`.
%% @since 2025-04-03
%% @complexity O(n), where n is the length of the username.
-spec register_user(binary(), binary(), {map(), map()}) -> {ok, {map(), map()}} | {error, binary()}.
register_user(Username, Password, {Trie, Users}) ->
    UsernameList = unicode:characters_to_list(Username),
    case contains(UsernameList, Trie) of
        true -> {error, "User already exists"};
        false ->
            {Salt, HashedPassword} = hash_password(Password),
            NewTrie = add(UsernameList, Trie),
            NewUsers = Users#{Username => #{password => {Salt, HashedPassword}, role => "user"}}, 
            {ok, {NewTrie, NewUsers}}
    end.

%% @doc Hashes a password using a secure algorithm.
%% This function generates a salt and applies a SHA-256 hash to the password.
%%
%% @param Password The password to hash.
%% @return A tuple `{Salt, HashedPassword}` where both values are binaries.
%% @since 2025-04-03
%% @complexity O(1)
-spec hash_password(binary()) -> {binary(), binary()}.
hash_password(Password) ->
    Salt = make_salt(),
    HashedPassword = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    {Salt, HashedPassword}.

%% @doc Generates a salt for password hashing.
%% !!! This is a placeholder function and should be replaced with a secure random generator. !!!
%%
%% @return A binary representing the salt.
%% @since 2025-04-03
%% @complexity O(1)
-spec make_salt() -> binary().
make_salt() ->
    <<"random_salt">>. 

%% @doc Verifies if a given password matches a stored hashed password.
%% This function re-hashes the input password using the provided salt
%% and checks if it matches the stored hashed password.
%%
%% @param Password The password input by the user.
%% @param Salt The salt used during password hashing.
%% @param HashedPassword The stored hashed password.
%% @return `true` if the password is correct, `false` otherwise.
%% @since 2025-04-03
%% @complexity O(1) 
-spec verify_password(binary(), binary(), binary()) -> boolean().
verify_password(Password, Salt, HashedPassword) ->
    HashedInput = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    HashedInput =:= HashedPassword.

%% @doc Logs in a user by verifying their password.
%% This function checks if the user exists and if the password matches 
%% the stored hashed password.
%%
%% @param Username The username as a binary string.
%% @param Password The password input.
%% @param AuthData A tuple `{Trie, Users}` representing the authentication data.
%% @return `{ok, "Login successful"}` if authentication succeeds, or an error tuple otherwise.
%% @since 2025-04-03
%% @complexity O(1) for user lookup, 
%% @complexity O(n) for password verification, where n is the length of the password.
-spec login_user(binary(), binary(), {map(), map()}) -> {ok, binary()} | {error, binary()}.
login_user(Username, Password, {_, Users}) ->
    case maps:find(Username, Users) of
        {ok, #{password := {Salt, HashedPassword}}} ->
            case verify_password(Password, Salt, HashedPassword) of
                true -> {ok, "Login successful"};
                false -> {error, "Invalid password"}
            end;
        error -> {error, "User not found"}
    end.







%%% ========================================================================================= %%%
%%%                                     Unit tests                                            %%%
%%% ========================================================================================= %%%
%% These tests verify user registration, login, and edge cases.
%% The tests use EUnit and cover valid/invalid scenarios.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Register User - Happy Path
register_user_happy_path_test_() ->
    InitialData = authentication_service:new(),
    {ok, Data1} = authentication_service:register_user("user1", "password123", InitialData),
    
    [
        ?_assertMatch({ok, _}, {ok, Data1}),  % Ensure registration succeeds
        ?_assert(authentication_service:contains("user1", element(1, Data1)))  % Check if user exists in Trie
    ].

%% Register User - Edge Cases
register_user_edge_cases_test_() ->
    InitialData = authentication_service:new(),
    {ok, Data1} = authentication_service:register_user("user1", "password123", InitialData),
    DuplicateUser = authentication_service:register_user("user1", "newpassword", Data1),  % Attempt duplicate registration
    
    [
        ?_assertEqual({error, "User already exists"}, DuplicateUser)  % Expect error
    ].

%% Login User - Happy Path
login_user_happy_path_test_() ->
    InitialData = authentication_service:new(),
    {ok, Data1} = authentication_service:register_user("user1", "password123", InitialData),
    LoginResult = authentication_service:login_user("user1", "password123", Data1),
    
    [
        ?_assertEqual({ok, "Login successful"}, LoginResult)  % Expect successful login
    ].

%% Login User - Edge Cases
login_user_edge_cases_test_() ->
    InitialData = authentication_service:new(),
    {ok, Data1} = authentication_service:register_user("user1", "password123", InitialData),
    
    WrongPasswordLogin = authentication_service:login_user("user1", "wrongpassword", Data1),
    NonExistentUserLogin = authentication_service:login_user("unknown_user", "password123", Data1),

    [
        ?_assertEqual({error, "Invalid password"}, WrongPasswordLogin),  % Wrong password
        ?_assertEqual({error, "User not found"}, NonExistentUserLogin)   % User does not exist
    ].

-endif. % End of TEST
%============================================================================================%