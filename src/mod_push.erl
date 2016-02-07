-module(mod_push).

-behaviour(gen_mod).

-export([start/2, stop/1, register_user/2, offline_message/3, muc_filter_message/5]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-record(phone_contacts,{user= <<"">>, phone = <<"">>, bare_phone = <<"">>}).


start(Host, Opts) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message, 49),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, muc_filter_message, 49),
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message, 49),
    ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, muc_filter_message, 49),
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50).

register_user(User, Server) ->
    MatchRec = #phone_contacts{user='$1', bare_phone = User, phone='$2'},
    Result = '$1',
    Res = mnesia:dirty_select(phone_contacts, [{MatchRec, [], [{{'$1','$2'}}]}]),
    notify_registered(jlib:jid_to_string(jlib:make_jid(User,Server,<<"">>)), Res).

notify_registered(_, []) ->
    ok;

notify_registered(Joined, [{Jid, PhoneId}|T]) ->
    Message = [
	       {<<"type">>,<<"registered">>},
	       {<<"phone">>, PhoneId},
	       {<<"jid">>, Joined}],
    mod_gcm:push(jlib:string_to_jid(Jid), jsx:encode(Message)),
    notify_registered(Joined, T).

offline_message(From, To, Packet) ->
    case fxml:get_tag_attr_s(<<"type">>,Packet) of
	<<"chat">> -> 
	    Message = [
		       {<<"type">>, <<"chat">>},
		       {<<"jid">>, jlib:jid_to_string(jlib:jid_remove_resource(From))},
		       {<<"message">>, fxml:get_path_s(Packet,[{elem, <<"body">>},cdata])}
		      ],

	    mod_gcm:push(To,jsx:encode(Message)),
	    ok;
	Type -> 
	    io:format("offline_message type: ~s",[Type]),
	    ok
    end. 

muc_filter_message(Pkt, #state{config = Config, jid=JID, users=Users, affiliations=Aff, subject=Subject} = MUCState, RoomJID, From, FromNick) ->
    OnlineUsers = lists:map(fun(JID) -> jlib:jid_remove_resource(JID) end,
			    dict:fetch_keys(Users)),
    AllUsers = dict:fetch_keys(Aff),
    OfflineUsers = AllUsers -- OnlineUsers,
    Message = [
	       {<<"type">>, <<"groupchat">>},
	       {<<"jid">>, jlib:jid_to_string(jlib:jid_remove_resource(From))},
	       {<<"message">>, fxml:get_path_s(Pkt, [{elem, <<"body">>}, cdata])},
	       {<<"room">>, jlib:jid_to_string(JID)}
	      ],
    lists:foreach(fun(User) ->
			  mod_gcm:push(User, jsx:encode(Message))
		  end,
		  OfflineUsers),
    Pkt.
