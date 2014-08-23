-module(sereal_decoder).

-export([decode/1, decode/2]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

decode(Data) ->
    decode(Data, []).

decode(Data, Opts) when is_binary(Data), is_list(Opts) ->
    case nif_decoder_init(Data, Opts) of
        {error, _} = Error ->
            throw(Error);
        
        {partial, ESereal} ->
            finish_decode(ESereal);
        
        {iter, Decoder, Objs, Curr} ->
            decode_loop(Data, Decoder, Objs, Curr);

        ESereal ->
            ESereal
    end;

decode(Data, Opts) when is_list(Data) ->
    decode(iolist_to_binary(Data), Opts).

finish_decode({bignum, Value}) ->
    list_to_integer(binary_to_list(Value));

finish_decode({bignum_e, Value}) ->
    {IVal, EVal} = case string:to_integer(binary_to_list(Value)) of

        {I, [$e | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E};

        {I, [$E | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E}
    end,

    IVal * math:pow(10, EVal);

finish_decode({bigdbl, Value}) ->
    list_to_float(binary_to_list(Value));

finish_decode({Pairs}) when is_list(Pairs) ->
    finish_decode_obj(Pairs, []);

finish_decode(Vals) when is_list(Vals) ->
    finish_decode_arr(Vals, []);

finish_decode(Val) ->
    maybe_map(Val).

maybe_map(Val) ->
    Val.

finish_decode_obj([], Acc) ->
    {lists:reverse(Acc)};
finish_decode_obj([{K, V} | Pairs], Acc) ->
    finish_decode_obj(Pairs, [{K, finish_decode(V)} | Acc]).

finish_decode_arr([], Acc) ->
    lists:reverse(Acc);
finish_decode_arr([V | Vals], Acc) ->
    finish_decode_arr(Vals, [finish_decode(V) | Acc]).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "sereal_decoder"), 0).


decode_loop(Data, Decoder, Objs, Curr) ->
    case nif_decoder_iterate(Data, Decoder, Objs, Curr) of
        {error, _} = Error ->
            throw(Error);

        {partial, ESereal} ->
            finish_decode(ESereal);
        
        {iter, NewDecoder, NewObjs, NewCurr} ->
            decode_loop(Data, NewDecoder, NewObjs, NewCurr);
        
        ESereal ->
            ESereal
    end.


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_decoder_init(_Data, _Opts) ->
    ?NOT_LOADED.

nif_decoder_iterate(_Data, _Decoder, _, _) ->
    ?NOT_LOADED.
