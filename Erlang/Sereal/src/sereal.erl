-module(sereal).

-export([encode/1, encode/2, decode/1, decode/2]).

encode(Data) ->
   encode(Data, []).

encode(Data, Opts) ->
    encoder:serealize(Data, Opts).

decode(Data) ->
   decode(Data, []).

decode(Data, Opts) ->
    decoder:deserealize(Data, Opts).
