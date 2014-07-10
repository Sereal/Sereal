-module(round_trip).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CASES, 
        [
        % numbers 
         1,
         0,
         128,
         2147483648,
         4294967296,
         1.111111,
         0.0000001,
         1.25,
         -1, 
         -2,
         -191,

       % atoms
         a,
         abcd,
         aabbccddeeffgghhii,
         zyxwvutsrqpon,

        % binaries
         <<"">>,
         <<"1">>,
         <<"\x45, \x52, \x4c, \x41, \x4E, \x47">>,
         <<"quick fox jumps over the lazy dog">>

         ]).

-define(LIST_CASES,
        [
            [],
            [1],
            [1,2,3],
            [[], []],
            [[1], [2], [3]],
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
        ]).

decode_encode(A) ->
    decode_encode(A, []).

decode_encode(A, DecodeOptions) ->
    decode_encode(A, DecodeOptions, []).

decode_encode(A, DecodeOptions, EncodeOptions) ->
    {ok, [Result]} = sereal:decode(sereal:encode(A, EncodeOptions), DecodeOptions),
    Result.

array_to_list(A) ->
    case array:is_array(A) of
         true -> array:to_list(array:map(fun (_, E) -> array_to_list(E) end, A));
         _    -> A
    end.

test_elem(A) ->
    test_elem(A, []).

test_elem(A, EncoderOptions) ->
    Z = decode_encode(A, [], EncoderOptions),
    B = array_to_list(Z),
    ?assertEqual(A, B).

list_test() ->
    [ test_elem(Case) || Case <- ?LIST_CASES ],
    [ test_elem(Case, [{snappy}]) || Case <- ?LIST_CASES ],
    [ test_elem(Case, [{zlib, 9}]) || Case <- ?LIST_CASES ],
    [ Case = decode_encode(Case, [{arrayref_to_list}]) || Case <- ?LIST_CASES ],
    [ Case = decode_encode(Case, [{arrayref_to_list}], [{snappy}]) || Case <- ?LIST_CASES ],
    [ Case = decode_encode(Case, [{arrayref_to_list}], [{zlib, 9}]) || Case <- ?LIST_CASES ].

atom_to_binary(A) ->
    list_to_binary(atom_to_list(A)).

test_case( Expected ) -> 
    test_case(Expected, []).

test_case( Expected, EncodeOptions ) -> 
    if is_atom(Expected) -> ?assertEqual(atom_to_binary(Expected), decode_encode(Expected, [], EncodeOptions));
       true -> ?assertEqual(Expected, decode_encode(Expected, [], EncodeOptions))
    end.
    
basic_test() -> 
    [test_case(Case) || Case <- ?TEST_CASES],
    [test_case(Case, [{snappy}])  || Case <- ?TEST_CASES],
    [test_case(Case, [{zlib, 9}]) || Case <- ?TEST_CASES].


-ifdef(SEREAL_MAP_SUPPORT).

-define(MAP_CASES, [
                    #{<<"a">> => 1},
                    #{<<"a">> => 1, <<"b">> => 2},
                    #{<<"a">> => #{<<"aa">> => #{<<"aaa">> => 123}}},
                    #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3, <<"d">> => 4, <<"e">> => 5, <<"f">> => 6, <<"g">> => 7, <<"h">> => 8, <<"i">> => 9, <<"j">> => 10, <<"k">> => 11, <<"l">> => 12, <<"m">> => 13, <<"n">> => 14, <<"o">> => 15, <<"p">> => 16, <<"q">> => 17, <<"r">> => 18, <<"s">> => 19, <<"t">> => 20, <<"u">> => 21, <<"v">> => 22, <<"w">> => 23, <<"x">> => 24, <<"y">> => 25, <<"z">> => 26, <<"a1">> => 27, <<"a2">> => 28, <<"a3">> => 29, <<"a4">> => 30, <<"a5">> => 31, <<"a6">> => 32, <<"a7">> => 33 }
                   ]).

maps_test() ->
    [ test_case(Case) || Case <- ?MAP_CASES ].

-endif.
