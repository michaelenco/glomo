-module(mod_muc_autojoin).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([start/2,
         stop/1,
	 muc_create/3,
	 muc_invite/3,
	 user_receive_packet/5,
	 muc_leave/3,
	 muc_list/3,
	 muc_members/3,
	 muc_subject/3,
	 user_online/3]).

-record(user_room, {key, status, timestamp}).
-record(archive_msg,
	{us = {<<"">>, <<"">>}                :: {binary(), binary()} | '$2',
	 id = <<>>                            :: binary() | '_',
	 timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_' | '$1',
	 peer = {<<"">>, <<"">>, <<"">>}      :: ljid() | '_' | '$3' | undefined,
	 bare_peer = {<<"">>, <<"">>, <<"">>} :: ljid() | '_' | '$3',
	 packet = #xmlel{}                    :: xmlel() | '_',
	 nick = <<"">>                        :: binary(),
	 type = chat                          :: chat | groupchat}).

-define(NS_MUC_CREATE, <<"muc:create">>).
-define(NS_MUC_INVITE, <<"muc:invite">>).
-define(NS_MUC_LEAVE, <<"muc:leave">>).
-define(NS_MUC_MEMBERS, <<"muc:members">>).
-define(NS_MUC_LIST, <<"muc:list">>).
-define(NS_MUC_SUBJECT, <<"muc:subject">>).

start(Host, Opts) -> 
    mnesia:create_table(user_room, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, user_room)}]),

    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, user_online, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, user_receive_packet, 50),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_CREATE, ?MODULE, muc_create, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_MEMBERS, ?MODULE, muc_members, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_LIST, ?MODULE, muc_list, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_LEAVE, ?MODULE, muc_leave, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_INVITE, ?MODULE, muc_invite, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_SUBJECT, ?MODULE, muc_subject, one_queue),
    ok.

stop(Host) ->
    ok.

user_online(_SID, #jid{user = User, server = Server}=JID, _Info) ->
    Rooms = mnesia:dirty_select(user_room, [{#user_room{key = {User,Server,'$1', '$2'}, 
					       status = <<"joined">>, _='_'},
				    [],
				    [{{'$1','$2'}}]}]),
    lists:foreach(fun({RoomId, RoomServer}) ->
			ejabberd_router:route(JID,
			      jid:make(RoomId, RoomServer, User),
			      #xmlel{name = <<"presence">>,
				      children = [
						  #xmlel{
						     name = <<"x">>,
						     attrs = [{<<"xmlns">>, ?NS_MUC}]
						    }
						 ]})
		 end,
		 Rooms),
    ok.

user_receive_packet(#xmlel{name = <<"message">>,
			   attrs = Attrs} = Packet
		    , _, JID, From, To) -> 
    case fxml:get_subtags_with_xmlns(Packet, <<"x">>, ?NS_MUC_USER) of
        [] -> ok;
	[X] ->
	    case fxml:get_subtag(X, <<"invite">>) of
		false -> ok;
		_ -> 
		    #jid{user = RoomId, server = RoomServer} = From,
		    #jid{user = User} = JID,
		    PresenceTo = jid:make(RoomId, RoomServer, User),
		    Presence = #xmlel{name = <<"presence">>,
			  children = [
				      #xmlel{
					 name = <<"x">>,
					 attrs = [{<<"xmlns">>, ?NS_MUC}]
					}
				     ]},
		    ejabberd_router:route(JID, PresenceTo, Presence)
	    end
    end,
    Packet;
user_receive_packet(Acc,_,_,_,_) -> Acc.

muc_create(#jid{user=User, server=Server}=From, _To, IQ) ->
    RoomId = p1_sha:sha(term_to_binary([From, 
					os:timestamp(),
					randoms:get_string()])),
    SubDomain = <<"conference.">>,
    RoomServer = <<SubDomain/binary,Server/binary>>,
    RoomJid = #jid{user=RoomId, server= RoomServer},

    PresenceTo = jid:make(RoomId, RoomServer, User),

    Presence = #xmlel{name = <<"presence">>,
			  children = [
				      #xmlel{
					 name = <<"x">>,
					 attrs = [{<<"xmlns">>, ?NS_MUC}]
					}
				     ]},

    ejabberd_router:route(From,PresenceTo,Presence),

    mnesia:dirty_write(#user_room{key = {User,Server,RoomId, RoomServer},
				status = <<"joined">>, timestamp = os:timestamp()}),

    IQ#iq{type = result, sub_el = [
			       #xmlel{
				  name = <<"new-room">>,
				  attrs = [{<<"jid">>, jlib:jid_to_string(RoomJid)}]
			      }]}.

muc_invite(#jid{user = User, server = Server} = From, 
	   To, 
	   #iq{sub_el = SubEl} = IQ) ->
    #xmlel{attrs=Attrs} = RoomTag = fxml:get_path_s(SubEl, [{elem, <<"room">>}]),
    #jid{user=RoomId, server=RoomServer} = RoomJid = jid:from_string(fxml:get_attr_s(<<"jid">>, Attrs)),
    UserList = fxml:get_subtags(RoomTag, <<"user">>),
    OnlineUsers = lists:map(fun(Str) -> 
				    jid:remove_resource(jid:from_string(Str))
			    end,
			    ejabberd_sm:connected_users()
			   ),
    Users = lists:map(fun(#xmlel{attrs = Attrs}) -> 
			      jid:remove_resource(
				jid:from_string(fxml:get_attr_s(<<"jid">>, Attrs)))
		      end,
		      UserList),
    IsMember = fun(Elem, List) ->
	[] /= [dummy || I<-List, Elem==I]
    end,

    OnlineInvitees = [X || X<-Users, IsMember(X, OnlineUsers)],

    lists:foreach(fun(#jid{user = User, server = Server} = JID) ->
	      Message = #xmlel{name = <<"message">>,
		   attrs = [{<<"to">>, jid:to_string(RoomJid)}],
		   children = [
		       #xmlel{name = <<"x">>,
			  attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
			  children = [
			      #xmlel{name = <<"invite">>,
				 attrs = [{<<"to">>, jid:to_string(jid:remove_resource(JID))}]}
			     ]}
		      ]},
	      ejabberd_router:route(From, RoomJid, Message),
	      mnesia:dirty_write(#user_room{key = {User,Server,RoomId, RoomServer},
					    status = <<"joined">>,
					    timestamp = os:timestamp()})
	  end,
	  Users),

    IQ#iq{type = result, sub_el = []}.

muc_leave(#jid{user = User, server = Server} = From, _To, #iq{sub_el = SubEl}=IQ) ->
    #xmlel{attrs=Attrs} = RoomTag = fxml:get_path_s(SubEl, [{elem, <<"room">>}]),
    #jid{user=RoomId, server=RoomServer} = jid:from_string(fxml:get_attr_s(<<"jid">>, Attrs)),
    io:format("~n~p~n~p~n~p~n~p~n~p~n",[From,User,Server,RoomId,RoomServer]),
    ejabberd_router:route(From,
			  jid:make(RoomId, RoomServer, User),
			  #xmlel{name = <<"presence">>,
				 attrs = [{<<"to">>, jid:to_string(jid:make(RoomId, RoomServer, User))},
					  {<<"type">>, <<"unavailable">>}]}),
    mnesia:dirty_write(#user_room{key = {User,Server,RoomId,RoomServer}, status = <<"leaved">>, timestamp = os:timestamp()}),
    IQ#iq{type = result, sub_el = []}.

muc_list(#jid{user = User, server = Server} = From, _To, IQ) ->
    Rooms = mnesia:dirty_select(user_room, [{#user_room{key = {User,Server,'$1', '$2'}, 
					       status = '$3', _='_'},
				    [],
				    [{{'$1','$2','$3'}}]}]),
    IQ#iq{type = result, 
	sub_el = [#xmlel{name = <<"room">>, 
			 attrs = [{<<"jid">>, jid:to_string(jid:make(RoomId, RoomServer, <<"">>))},
				  {<<"status">>, Status},
                                                {<<"subject">>,get_muc_subject(RoomId,RoomServer)}], 
                                     children = xmlMucMembers(RoomId,RoomServer)}  
		  || {RoomId, RoomServer,Status} <- Rooms]}.

muc_members(#jid{user = User, server = Server} = From, _To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{attrs=Attrs} = RoomTag = fxml:get_path_s(SubEl, [{elem, <<"room">>}]),
    #jid{user=RoomId, server=RoomServer} = jid:from_string(fxml:get_attr_s(<<"jid">>, Attrs)),
    Members = xmlMucMembers(RoomId,RoomServer),
    IQ#iq{type = result, sub_el = Members}.

xmlMucMembers(RoomId,RoomServer)->
    Users = mnesia:dirty_select(user_room, [{#user_room{key = {'$1', '$2',RoomId, RoomServer}, 
                           status = '$3', _='_'},
                    [],
                    [{{'$1','$2','$3'}}]}]),
    [#xmlel{name = <<"user">>, 
             attrs = [{<<"jid">>, jid:to_string(jid:make(User, Server, <<"">>))},
                  {<<"status">>, Status}]}  
          || {User, Server ,Status} <- Users].

is_subject_message(#xmlel{name = <<"message">>, children=Children}) ->
    Subjects = lists:filter(fun(X) ->
				    case X of
					#xmlel{name = <<"subject">>} ->
					    true;
					_ -> false
				    end
			    end,
			    Children),
    length(Subjects) > 0;
is_subject_message(_) -> false.

muc_subject(_From, _To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{attrs=Attrs} = RoomTag = fxml:get_path_s(SubEl, [{elem, <<"room">>}]),
    #jid{user=RoomId, server=RoomServer} = jid:from_string(fxml:get_attr_s(<<"jid">>, Attrs)),
    Subject = get_muc_subject(RoomId,RoomServer),
    IQ#iq{type = result, sub_el = [{xmlcdata, Subject}]}.
    

get_muc_subject(RoomId,RoomServer)->
    Messages = mnesia:dirty_select(archive_msg, [{#archive_msg{
                             us = {RoomId,RoomServer},
                             packet = '$1',
                             _ = '_'},
                          [],
                          ['$1']}]),
    SubjectMessages = lists:filter(fun is_subject_message/1, Messages),
    
    if
    length(SubjectMessages) > 0 ->
        Index = length(SubjectMessages),
        LastSubject = lists:nth(Index, SubjectMessages),
        fxml:get_path_s(LastSubject, [{elem, <<"subject">>}, cdata]);          
    true ->
        "none"
    end.    




