-module(helpers).
-behaviour(gen_mod).
-export([start/2, stop/1, show_table/1, get_user_country/1,return_text_format/1,map_into_to_hex/1,make_glomo_url/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).

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

get_user_country(User) ->
     case mnesia:dirty_read(user_countries, User) of
        [Country] ->
            {ok, Country};
        [] ->
            {error, notfound}
    end.
    

return_text_format(Text) ->
    text_schema(Text,false).

text_schema([], Flag) ->
    if 
        Flag == true ->
            unicode;
        true ->
            norm
    end;

text_schema([H|T],Flag) ->
     if 
        H > 255 ->
            text_schema(T,true);
        true ->
            text_schema(T,Flag)
    end.

make_glomo_url(Url) ->
    URLHex = lists:concat(lists:map(fun(X) -> helpers:map_into_to_hex(X) end,Url)),
    {UrlGlomo,_} = lists:foldl(fun(Element,Accum) -> glomo_url(Element, Accum) end, {"",0}, URLHex),
    UrlGlomo.

map_into_to_hex(X) ->
    if
        is_integer(X) == true ->
            Hexed = integer_to_list(X,16),
            add_zeroes_to_hex(Hexed);
        true ->
            X
    end.

add_zeroes_to_hex(X) ->
    if
        length(X) < 4 ->
            add_zeroes_to_hex(lists:append("0", X));
        true ->
            X   
    end.

glomo_url(Element,{Result,Cnt} = Accum) ->
    NewCnt = Cnt +1,
    if Cnt rem 2 == 0 ->
        NewResult = lists:append(Result,"%"),
        ReturnResult = lists:append(NewResult, [Element]),
        {ReturnResult, NewCnt};
   true->
        ReturnResult = lists:append(Result,[Element]),
        {ReturnResult,NewCnt}
end.


