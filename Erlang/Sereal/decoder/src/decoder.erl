-module(decoder).

-export([deserealize/2]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "decoder/priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

deserealize(Data, Opts) when is_binary(Data), is_list(Opts) ->
    case nif_decoder_init(Data, Opts) of
        {error, _} = Error ->
            throw(Error);
        
        {partial, Decoder, Objs, Curr} ->
            decode_loop(Data, Decoder, Objs, Curr);

	%% Special case: the iteration only decompressed the payload and
	%% returns the new uncompressed data. Let's use this and re-iterate
        {partial, Decoder, Objs, Curr, NewData} ->
            decode_loop(NewData, Decoder, Objs, Curr);
    
    % when c_code ask to create module-based data structures, currently on for arrays module
        {convert, NewDecoder, NewObjs, NewCurr} -> 
            A = array:from_list(lists:reverse(NewCurr)),
            [ Head | NewObjs2 ] = NewObjs,
            NewCurr2 = [A | Head],
            decode_loop(Data, NewDecoder, NewObjs2, NewCurr2);

        ESereal ->
            ESereal
    end.

decode_loop(Data, Decoder, Objs, Curr) ->
    case nif_decoder_iterate(Data, Decoder, Objs, Curr) of
        {error, _} = Error ->
            throw(Error);

        {partial, NewDecoder, NewObjs, NewCurr} ->
            decode_loop(Data, NewDecoder, NewObjs, NewCurr);

	%% Special case: the iteration only decompressed the payload and
	%% returns the new uncompressed data. Let's use this and re-iterate
        {partial, NewDecoder, NewObjs, NewCurr, NewData} ->
            decode_loop(NewData, NewDecoder, NewObjs, NewCurr);
        
    % when c_code asks to create module-based data structures, currently used only for arrays module
        {convert, NewDecoder, NewObjs, NewCurr} -> 
            A = array:from_list(lists:reverse(NewCurr)),
            [ Head | NewObjs2 ] = NewObjs,
            NewCurr2 = [A | Head],
            decode_loop(Data, NewDecoder, NewObjs2, NewCurr2);

        ESereal ->
            ESereal
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_decoder_init(_, _) ->
    ?NOT_LOADED.

nif_decoder_iterate(_, _, _, _) ->
    ?NOT_LOADED.
