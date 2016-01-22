-module(helpers).
-behaviour(gen_mod).
-export([start/2, stop/1, show_table/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
	ok.

stop(Host) ->
	ok.

show_table(Table_name) ->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.