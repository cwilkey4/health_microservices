-module(anonymizer_tests).
-include_lib("eunit/include/eunit.hrl").



% Test for mask_email/1
mask_email_test() ->
    ?assertEqual("**hn.doe@example.com", anonymizer:mask_email("john.doe@example.com")),
    ?assertEqual("**m@domain.com", anonymizer:mask_email("sam@domain.com")),
    ?assertEqual("Invalid Email", anonymizer:mask_email("invalid-email")).

% Test for pseudonymize_name/1
pseudonymize_name_test() ->
    Hash1 = anonymizer:pseudonymize_name("John Doe"),
    Hash2 = anonymizer:pseudonymize_name("Jane Doe"),
    ?assertNotEqual(Hash1, Hash2), % Different names should produce different hashes
    ?assert(is_binary(Hash1)), % Ensure the output is a binary

    % Repeated calls with the same input should give the same hash
    ?assertEqual(Hash1, anonymizer:pseudonymize_name("John Doe")).

% Test for generalize_dob/1
generalize_dob_test() ->
    ?assertEqual("Under 18", anonymizer:generalize_dob("2010-05-12")),
    ?assertEqual("18-35", anonymizer:generalize_dob("1995-03-08")),
    ?assertEqual("36-60", anonymizer:generalize_dob("1970-07-20")),
    ?assertEqual("60+", anonymizer:generalize_dob("1945-11-30")).
