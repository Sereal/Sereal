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

        % binaries
         <<"">>,
         <<"1">>,
         <<"\x45, \x52, \x4c, \x41, \x4E, \x47">>,
         <<"quick fox jumps over the lazy dog">>

         ]).

-define(LIST_CASES,
        [
            [1],
            [1,2,3],
            [[], []],
            [[1], [2], [3]]
        ]).

decode_encode(A) ->
    {ok, [Result]} = sereal:decode(sereal:encode(A)),
    Result.

array_to_list(A) ->
    case array:is_array(A) of
         true -> array:to_list(array:map(fun (_, E) -> array_to_list(E) end, A));
         _    -> A
    end.

test_elem(A) ->
    Z = decode_encode(A),
    B = array_to_list(Z),
    ?assertEqual(A, B).

list_test() ->
    [ test_elem(Case) || Case <- ?LIST_CASES ].

test_case( Expected ) -> 
    ?assertEqual(Expected, decode_encode(Expected)).
    
basic_test() -> 
    [test_case(Case) || Case <- ?TEST_CASES].


-ifdef(SEREAL_MAP_SUPPORT).

-define(MAP_CASES, [
                    #{<<"a">> => 1},
                    #{<<"a">> => 1, <<"b">> => 2},
                    #{<<"a">> => #{<<"aa">> => #{<<"aaa">> => 123}}},
                    #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3, <<"d">> => 4, <<"e">> => 5, <<"f">> => 6, <<"g">> => 7, <<"h">> => 8, <<"i">> => 9, <<"j">> => 10, <<"k">> => 11, <<"l">> => 12, <<"m">> => 13, <<"n">> => 14, <<"o">> => 15, <<"p">> => 16, <<"q">> => 17, <<"r">> => 18, <<"s">> => 19, <<"t">> => 20, <<"u">> => 21, <<"v">> => 22, <<"w">> => 23, <<"x">> => 24, <<"y">> => 25, <<"z">> => 26, <<"t">> => 20, <<"u">> => 21, <<"v">> => 22, <<"w">> => 23, <<"x">> => 24, <<"y">> => 25, <<"z">> => 26 }
                   ]).

maps_test() ->
    [ test_case(Case) || Case <- ?MAP_CASES ].

-endif.
