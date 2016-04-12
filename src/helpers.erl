-module(helpers).
-behaviour(gen_mod).
-export([start/2, stop/1, show_table/1, get_user_country/1,return_text_format/1,map_into_to_hex/1,make_glomo_url/1,get_user_nickname/2]).

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



%Get user nickname from vcard
get_user_nickname(User,Server)->
    ModVcard = get_vcard_module(Server),
    get_rosteritem_name(ModVcard, User, Server).

get_vcard_module(Server) ->
    Modules = gen_mod:loaded_modules(Server),
    [M
     || M <- Modules,
    (M == mod_vcard) or (M == mod_vcard_ldap)].

get_rosteritem_name([], _, _) -> <<"">>;
get_rosteritem_name([ModVcard], U, S) ->
    From = jid:make(<<"">>, S, jlib:atom_to_binary(?MODULE)),
    To = jid:make(U, S, <<"">>),
    case lists:member(To#jid.lserver, ?MYHOSTS) of
        true ->
            IQ = {iq, <<"">>, get, <<"vcard-temp">>, <<"">>,
                  #xmlel{name = <<"vCard">>,
                         attrs = [{<<"xmlns">>, <<"vcard-temp">>}],
                         children = []}},
            IQ_Vcard = ModVcard:process_sm_iq(From, To, IQ),
            case catch get_rosteritem_name_vcard(IQ_Vcard#iq.sub_el) of
                {'EXIT', Err} ->
                    ?ERROR_MSG("Error found when trying to get the "
                               "vCard of ~s@~s in ~p:~n ~p",
                               [U, S, ModVcard, Err]),
                    <<"">>;
                NickName ->
                    NickName
            end;
        false ->
            <<"">>
    end.

get_rosteritem_name_vcard([]) -> <<"">>;
get_rosteritem_name_vcard([Vcard]) ->
    case fxml:get_path_s(Vcard,
            [{elem, <<"NICKNAME">>}, cdata])
    of
      <<"">> ->
      fxml:get_path_s(Vcard, [{elem, <<"FN">>}, cdata]);
      Nickname -> Nickname
    end.


