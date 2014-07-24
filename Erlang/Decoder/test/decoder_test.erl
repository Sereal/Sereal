-module(decoder_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    DIR = "../test/",

    {ok, Data} = file:read_file(DIR ++ "test.srl"),
    {ok, [ Struct ]} = sereal_decoder:decode(Data),

    [{[{<<"a">>,1}]},{[{<<"b">>,2}]}] = Struct,

    {ok, Data2} = file:read_file(DIR ++ "test2.srl"),
    {ok, [ Struct2 ]} = sereal_decoder:decode(Data2),

    [2, [1, <<"foo">>, <<"bar">>]] = Struct2,

    {ok, Data3} = file:read_file(DIR ++ "test3.srl"),
    {ok, [ Struct3 ]} = sereal_decoder:decode(Data3),

    {[{<<"foo">>, <<"bar">>}]} = Struct3,
    
    {ok, Data4} = file:read_file(DIR ++ "test4.srl"),
    {ok, [ Struct4 ]} = sereal_decoder:decode(Data4),

    {[{<<"more">>, [1,2,3,4,5]},
      {<<"data">>, [{[
                      { <<"a">>, <<"b">>},
                      { <<"c">>, <<"d">>},
                      { <<"e">>, <<"f">>}]},
                    {[{ <<"foo">>, 4.2 }]}]}]} = Struct4, 

    
    {ok, Data5} = file:read_file(DIR ++ "test5.srl"),
    {ok, [ Struct5 ]} = sereal_decoder:decode(Data5).
    

