-module(anonymizer).
-export([mask_email/1, pseudonymize_name/1, generalize_dob/1]).


%% Mask email address by replacing part of the username with asterisks
mask_email(Email) ->
    case string:split(Email, "@") of
        [User, Domain] ->
            MaskedUser = lists:duplicate(length(User) - 2, $*) ++ lists:sublist(User, length(User) - 1, 2),
            MaskedUser ++ "@" ++ Domain;
        _ -> "Invalid Email"
    end.

%% Pseudonymization of Name using MD5 Hash
pseudonymize_name(Name) ->
    Hash = crypto:hash(md5, Name), % Generate MD5 hash
    base64:encode(Hash).           % Convert hash to Base64

%% Generalize Date of Birth into Age Brackets
generalize_dob(DOB) ->
    {Y, _, _} = calendar:universal_time(),
    Age = Y - list_to_integer(string:sub_string(DOB, 1, 4)),
    case Age of
        X when X < 18 -> "Under 18";
        X when X >= 18, X =< 35 -> "18-35";
        X when X > 35, X =< 60 -> "36-60";
        _ -> "60+"
    end.
