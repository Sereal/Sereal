-module(basic_tests).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    Cases = read_cases("all"),
    [gen(Case) || Case <- Cases].

arrayref_to_list_test() ->
    Cases = read_cases("arrayref_list"),
    [gen(Case, [{arrayref_to_list}]) || Case <- Cases].

-ifdef(SEREAL_MAP_SUPPORT).

maps_map_test() ->
    Cases = read_cases("maps_map"),
    [gen(Case) || Case <- Cases].

-else.

maps_tuple_test() ->
    Cases = read_cases("maps_tuple"),
    [gen(Case) || Case <- Cases].

-endif.

gen({Name, Srl, {error, _}=Erl}) ->
   {Name, ?_assertThrow(Erl, sereal:decode(Srl))};

gen({Name, Srl, Erl}) ->
    {ok, Decodeds} = sereal:decode(Srl),
    [Decoded | _ ] = Decodeds,
    {Name, ?assertEqual(Erl, Decoded)}.

gen({Name, Srl, Erl}, DecoderOpts) ->
    {ok, Decodeds} = sereal:decode(Srl, DecoderOpts),
    [Decoded | _ ] = Decodeds,
    {Name, ?assertEqual(Erl, Decoded)}.

read_cases(Dirname) ->
    CasesPath = filename:join(["..", "test", "cases", Dirname, "*.srl"]),
    FileNames = lists:sort(filelib:wildcard(CasesPath)),
    lists:map(fun(F) -> make_pair(F) end, FileNames).

make_pair(FileName) ->
    {ok, Srl} = file:read_file(FileName),
    BaseName = filename:rootname(FileName),
    ErlFname = BaseName ++ ".eterm",
    {ok, [Term]} = file:consult(ErlFname),
    {filename:basename(BaseName), Srl, Term}.
