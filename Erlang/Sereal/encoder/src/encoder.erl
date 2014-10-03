-module(encoder).

-export([serealize/2]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "encoder/priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

serealize(Data, Opts) ->
    case srl_encoder_setup([Data], Opts) of
        {error, _} = Error ->
            throw(Error);

        {error, _, _} = Error ->
            throw(Error);
        
        {partial, Items, Encoder} ->
            encoder_loop(Items, Encoder)

    end.

encoder_loop(Items, Encoder) ->
    case srl_encoder_parse(Items, Encoder) of 
        {error, _} = Error->
            throw(Error);

        {error, _, _} = Error ->
            throw(Error);

        {convert, NewItems, NewEncoder, Term} ->
            NewTerm = term_to_list(Term),
            NewItems2 = [NewTerm | NewItems],
            encoder_loop(NewItems2, NewEncoder);

        {partial, NewItems, NewEncoder} ->
            encoder_loop(NewItems, NewEncoder);

        EncoderBinary ->
            EncoderBinary
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

srl_encoder_setup(_, _) ->
    ?NOT_LOADED.

srl_encoder_parse(_, _) ->
    ?NOT_LOADED.

term_to_list(Term) ->
    case array:is_array(Term) of
        true -> array:to_list(Term);
           _ -> tuple_to_list(Term)
    end.
