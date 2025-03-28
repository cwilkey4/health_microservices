-module(name_autocomplete).
-export([add/2, remove/2]).


add([], Trie) ->
    maps:put(last, true, Trie);
add([H|T], Trie) ->
    Strie = maps:get(H, Trie, #{}),
    Nstrie = add(T, Strie),
    maps:put(H, Nstrie, Trie).

remove([], Trie) ->
    maps:remove(last, Trie);
remove([H|T], Trie) ->
    case maps:find(H, Trie) of
        {ok, Strie} ->
            Nstrie = remove(T, Strie),
            case maps:size(Nstrie) of
                0 -> maps:remove(H, Trie);
                _ -> maps:put(H, Nstrie, Trie)
            end;
        error -> Trie
    end.