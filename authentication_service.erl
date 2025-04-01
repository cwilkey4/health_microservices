%-----------------------------------------------------------------------------------------------%
% Start the Erlang shell
% erl

% Compile the module
% c(authentication_service).

% Initialize the data structure
% Data = authentication_service:new().

% Register a user
% {ok, Data1} = authentication_service:register_user("user1", "password123", Data).

% Log in with correct credentials
% authentication_service:login_user("user1", "password123", Data1).

% Log in with incorrect credentials
% authentication_service:login_user("user1", "wrongpassword", Data1).

% Log in with a non-existent user
% authentication_service:login_user("unknown_user", "password123", Data1).

% Exit the shell
% q().

%-----------------------------------------------------------------------------------------------% 

-module(authentication_service).
-export([new/0, add/2, contains/2, register_user/3, login_user/3, hash_password/1, verify_password/3]).

% Initialize an empty Trie and User Store
new() -> {#{}, #{}}.  % {Trie, Users}

% Add a list of elements (username) to the Trie
add([], Trie) -> Trie#{"end" => true};
add([H | T], Trie) ->
    SubTrie = maps:get(H, Trie, #{}),
    Trie#{H => add(T, SubTrie)}.

% Check if a username exists in the Trie
contains([], Trie) -> maps:is_key("end", Trie);
contains([H | T], Trie) ->
    case maps:find(H, Trie) of
        {ok, SubTrie} -> contains(T, SubTrie);
        error -> false
    end.

% Register a new user
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

% Hash the password using a secure algorithm (replace with a real hashing function)
hash_password(Password) ->
    Salt = make_salt(),
    HashedPassword = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    {Salt, HashedPassword}.

% Generate a salt (dummy implementation, replace with proper random generation)
make_salt() ->
    <<"random_salt">>. 

% Verify the password
verify_password(Password, Salt, HashedPassword) ->
    HashedInput = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    HashedInput =:= HashedPassword.

% User login
login_user(Username, Password, {_, Users}) ->
    case maps:find(Username, Users) of
        {ok, #{password := {Salt, HashedPassword}}} ->
            case verify_password(Password, Salt, HashedPassword) of
                true -> {ok, "Login successful"};
                false -> {error, "Invalid password"}
            end;
        error -> {error, "User not found"}
    end.
