-module(sereal).

-export([decode/1, decode/2, encode/1, encode/2]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

decode(Data) ->
    decode(Data, []).

decode(Data, Opts) when is_binary(Data), is_list(Opts) ->
    case nif_decoder_init(Data, Opts) of
        {error, _} = Error ->
            throw(Error);
        
        {iter, Decoder, Objs, Curr} ->
            decode_loop(Data, Decoder, Objs, Curr);

	%% Special case: the iteration only decompressed the payload and
	%% returns the new uncompressed data. Let's use this and re-iterate
        {iter, Decoder, Objs, Curr, NewData} ->
            decode_loop(NewData, Decoder, Objs, Curr);

        ESereal ->
            ESereal
    end.

decode_loop(Data, Decoder, Objs, Curr) ->
    case nif_decoder_iterate(Data, Decoder, Objs, Curr) of
        {error, _} = Error ->
            throw(Error);

        {iter, NewDecoder, NewObjs, NewCurr} ->
            decode_loop(Data, NewDecoder, NewObjs, NewCurr);

	%% Special case: the iteration only decompressed the payload and
	%% returns the new uncompressed data. Let's use this and re-iterate
        {iter, NewDecoder, NewObjs, NewCurr, NewData} ->
            decode_loop(NewData, NewDecoder, NewObjs, NewCurr);
        
        ESereal ->
            ESereal
    end.

encode(Data) ->
    encode(Data, {}).

encode(Data, Opts) ->
    case nif_encoder_init([Data], Opts) of
        {error, _} = Error ->
            throw(Error);
        
        {iter, Items, Encoder} ->
            io:format("Iteraete: ~p => ~p~n", [Items, Encoder]),
            encoder_loop(Items, Encoder);

        EncodedBinary ->
            EncodedBinary
    end.

encoder_loop(Items, Encoder) ->
    case nif_encoder_iterate(Items, Encoder) of 
        {error, Reason} = Error->
            throw(Error);

        {iter, NewItems, NewEncoder} ->
            encoder_loop(NewItems, NewEncoder);

        EncoderBinary ->
            io:format("Finished~n"),
            EncoderBinary
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_decoder_init(_, _) ->
    ?NOT_LOADED.

nif_decoder_iterate(_, _, _, _) ->
    ?NOT_LOADED.

nif_encoder_init(_, _) ->
    ?NOT_LOADED.

nif_encoder_iterate(_, _) ->
    ?NOT_LOADED.

