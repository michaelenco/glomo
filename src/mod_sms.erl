-module(mod_sms).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([start/2,
         stop/1]).

-export([filter_packet/1]).


start(Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50).

stop(Host) ->
	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50).

filter_packet(P) ->
	{From,To,Message} = P,
	{xmlel, Name, _Attrs, _Els} = Message,
	if Name == <<"message">> ->
			io:format("Filter packet:~p~n", [Message]);
		true -> ok
	end,
	P.

