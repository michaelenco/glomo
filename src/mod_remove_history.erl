-module(mod_remove_history).

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,ultra_dirty_select/1,show_dirty_pattern/0,process_clear_iq/3,ultra_dirty_select_with/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

%REMOVE - удаление сообщения из исстории, Clear - очистка истории
-define(NS_REMOVE_HISTORY, <<"jabber:iq:remove_history">>). 
-define(NS_CLEAR_HISTORY, <<"jabber:iq:clear_history">>). 


start(Host, Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_REMOVE_HISTORY, ?MODULE, process_local_iq, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_CLEAR_HISTORY, ?MODULE, process_clear_iq, one_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_REMOVE_HISTORY),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_CLEAR_HISTORY).

process_clear_iq(#jid{user=User, server=Server}= From,_To,#iq{type=set, sub_el = SubEl} = IQ) -> 
    #xmlel{attrs=Attrs} = SubEl,
    WithS = fxml:get_attr_s(<<"with">>, Attrs),
    With = jid:from_string(WithS),
    Messages = ultra_dirty_select_with(From,With),
    lists:foreach(fun(Rec) ->
              ok = mnesia:dirty_delete_object(Rec)
          end, Messages),
    Result = #xmlel{name = <<"result">>, attrs = [{<<"status">>,<<"ok">>},{<<"count">>,list_to_binary(integer_to_list(length(Messages)))}]},
    IQ#iq{type = result,sub_el = [Result]}.

process_local_iq(From,_To,#iq{type=set, sub_el = SubEl} = IQ) -> 
    Tag = fxml:get_subtag(SubEl, <<"id">>),
    {xmlel,_,_,Children} = Tag,
    MessageId = binary_to_list(fxml:get_cdata(Children)),
    Messages = ultra_dirty_select(MessageId),
    lists:foreach(fun(Rec) ->
              ok = mnesia:dirty_delete_object(Rec)
          end, Messages),
    Result = #xmlel{name = <<"result">>, attrs = [{<<"status">>,<<"ok">>}]},
    IQ#iq{type = result,sub_el = [Result]}.


%Partly generated by mnesia:table_info(Tab,wild_pattern).
-record(archive_msg,
    {us = '_',
     id = '_',
     timestamp = '_',
     peer = '_',
     bare_peer = '_',
     packet = #xmlel{name = <<"message">>, attrs ='_', children = '_'},
     nick = '_',
     type = chat}).

show_dirty_pattern() ->
            io:format("~n~n ~p ~n~n",[#archive_msg{packet = #xmlel{name = <<"message">>,attrs = ['_','_','_',{<<"id">>,'_'},'_','_'],children = '_'}}]).

ultra_dirty_select(ID) ->
    Secret = mnesia:dirty_match_object(#archive_msg{packet = #xmlel{name = <<"message">>,attrs = ['_','_','_',{<<"id">>,list_to_binary(ID)},'_','_'],children = '_'}}),
            Chat = mnesia:dirty_match_object(#archive_msg{packet = #xmlel{name = <<"message">>,attrs = ['_','_','_',{<<"id">>,list_to_binary(ID)},'_'],children = '_'}}),
            lists:append(Secret,Chat).

ultra_dirty_select_with(From,With) ->
           #jid{user = FromU, server = FromS} = From,
           #jid{user = WithU, server = WithS} = With,
           mnesia:dirty_match_object(#archive_msg{us = {FromU,FromS},bare_peer = {WithU,WithS,'_'}}).
