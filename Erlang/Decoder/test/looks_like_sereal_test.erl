-module(looks_like_sereal_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CASES, 
        %    input                                        error message
        [ 
          { <<"">>,                                      'Sereal lacks data' },
          { <<"=srl">>,                                  'Sereal lacks data'},
          { <<"=srl\x03\x00\x25">>,                      'Unsupported Sereal protocol'},
          { <<"=\xF3rl\x02\x00\x25">>,                   'Unsupported Sereal protocol'},
          { <<"=\xF3rl\x00\x00\x25">>,                   'Unsupported Sereal protocol'},
          { <<"undefined">>,                             'Wrong magic string for Sereal'},
          { <<"\x3d\x73\x72\x6c\x02\x00\x0f,\x11\x11">>, 'Wrong structured Sereal'}
        ]).

wrong_formatted_sereals_test() ->
    lists:map(fun ({Input, Msg}) -> 
                   ?assertThrow({error, {1, Msg}},
                                sereal_decoder:decode(Input))
             end,
             ?TEST_CASES).  
