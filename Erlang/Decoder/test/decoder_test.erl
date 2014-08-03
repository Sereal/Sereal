-module(decoder_test).

-include_lib("eunit/include/eunit.hrl").

file_test() ->
    Cases = read_cases(),
    [gen(Case) || Case <- Cases].

gen({Name, Srl, {error, _}=Erl}) ->
    {Name, ?_assertThrow(Erl, sereal_decoder:decode(Srl))};

gen({Name, Srl, Erl}) ->
    {ok, Decodeds} = sereal_decoder:decode(Srl),
    [Decoded | _ ] = Decodeds,
    {Name, ?assertEqual(Erl, Decoded)}.

read_cases() ->
    CasesPath = filename:join(["..", "test", "cases", "*.srl"]),
    FileNames = lists:sort(filelib:wildcard(CasesPath)),
    lists:map(fun(F) -> make_pair(F) end, FileNames).

make_pair(FileName) ->
    {ok, Srl} = file:read_file(FileName),
    BaseName = filename:rootname(FileName),
    ErlFname = BaseName ++ ".eterm",
    {ok, [Term]} = file:consult(ErlFname),
    {filename:basename(BaseName), Srl, Term}.
