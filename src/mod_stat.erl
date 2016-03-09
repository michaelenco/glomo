-module(mod_stat).

-behaviour(gen_mod).

-export([start/2, stop/1, 
	 register_user/2, 
	 init_mcc_table/1, 
	 user_sent_sms/2, 
	 http_upload_slot_request/5,
	 webadmin_json_api/2,
	 user_send_packet/4,
	 generate_stat_data/0,
	 session_close/1,
	 user_sent_invite/2,
	 remove_stat_data/0]).  

-include("ejabberd.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).
-record(phone_contacts,{user, phone, bare_phone, joined}).


-record(stat_register, {user, timestamp}).
-record(mcc_country, {mcc, country}).
-record(stat_sms, {user, timestamp}).
-record(stat_session, {user, start, length, country}).
-record(stat_file, {user, size, type, timestamp, country}).
-record(stat_chat, {user, timestamp, country}).
-record(stat_invites,{user= <<"">>, phone = <<"">>, timestamp}).
-record(session, {sid,usr,us,priority,info}).

-record(last_activity, {us, timestamp, status}).  

start(Host, _Opts) ->
    mnesia:create_table(mcc_country, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, mcc_country)}]),
    mnesia:create_table(stat_sms, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_sms)}]),
    mnesia:create_table(stat_register, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, stat_register)}]),
    mnesia:create_table(stat_file, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_file)}]),
    mnesia:create_table(stat_chat, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_chat)}]),
    mnesia:create_table(stat_session, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_session)}]),
    mnesia:create_table(stat_invites, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_invites)}]),

    ejabberd_hooks:add(session_close, global, ?MODULE, session_close, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 50),
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:add(http_upload_slot_request, Host, ?MODULE, http_upload_slot_request, 50),
    ejabberd_hooks:add(sms_sent, global, ?MODULE, user_sent_sms, 50),
    ejabberd_hooks:add(invite_sent, global, ?MODULE, user_sent_invite, 50),
    ejabberd_hooks:add(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(session_close, Host, ?MODULE, user_send_packet, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 50),
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:delete(http_upload_slot_request, Host, ?MODULE, http_upload_slot_request, 50),
    ejabberd_hooks:delete(sms_sent, Host, ?MODULE, user_sent_sms, 50),
    ejabberd_hooks:delete(invite_sent, Host, ?MODULE, user_sent_invite, 50),

    ejabberd_hooks:delete(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

now_to_timestamp({MegaSec, Sec, MiliSec}) ->
    MegaSec*1000000000+Sec*1000+(MiliSec div 1000).

webadmin_json_api(Acc, #request{path = [<<"api">>, <<"stat">>]}) -> 
    UserQuery = fun() -> 
	  Q = qlc:q([
		     [{<<"user">>,User},
		      {<<"timestamp">>,now_to_timestamp(Now)},
		      {<<"country">>, Country},
		      {<<"last_activity">>, LT*1000}] ||
		     #stat_register{user=User, timestamp=Now} <- mnesia:table(stat_register),
		     #user_countries{user=CUser, country=CMcc} <- mnesia:table(user_countries),
		     User == CUser,
		     #mcc_country{mcc=Mcc, country=Country} <- mnesia:table(mcc_country),
		     Mcc == CMcc,
		     #last_activity{us={LU,_}, timestamp=LT} <- mnesia:table(last_activity),
		     User == LU]),	
	  qlc:e(Q)
    end,
    SmsQuery = fun() ->
	  Q = qlc:q([
		   [{<<"timestamp">>, now_to_timestamp(Now)},
		    {<<"country">>, Country}] ||  
		    #stat_sms{user=User, timestamp=Now} <-mnesia:table(stat_sms),
		    #user_countries{user=CU, country=CMcc} <-mnesia:table(user_countries),
		    User == CU,
		    #mcc_country{mcc=Mcc, country=Country} <- mnesia:table(mcc_country),
		    Mcc==CMcc]),
	  qlc:e(Q)
    end,
    FileQuery = fun() ->
	    Q = qlc:q([
		       [{<<"timestamp">>, now_to_timestamp(Now)},
			{<<"country">>, Country},
			{<<"size">>, Size},
			{<<"type">>, Type},
			{<<"user">>, User}] ||  
		       #stat_file{user=User, 
				  timestamp=Now,
				  country=Country,
				  size=Size,
				  type=Type} <-mnesia:table(stat_file)
		      ]),
	    qlc:e(Q)
    end,
    ChatQuery = fun() ->
	    Q = qlc:q([
		       [{<<"timestamp">>, now_to_timestamp(Now)},
			{<<"country">>, Country},
			{<<"user">>, User}] ||  
		       #stat_chat{user=User, 
				  timestamp=Now,
				  country=Country} <-mnesia:table(stat_chat)
		      ]),
	    qlc:e(Q)
    end,
    SessionQuery = fun() ->
	    Q = qlc:q([
		       [{<<"start">>, now_to_timestamp(Now)},
			{<<"country">>, Country},
			{<<"length">>, Length},
			{<<"user">>, User}] ||  
		       #stat_session{user=User, 
				  start=Now,
				  length=Length,
				  country=Country} <-mnesia:table(stat_session)
		      ]),
	    qlc:e(Q)
    end,
    InviteQuery = fun() ->
	    Q = qlc:q([
		       [{<<"timestamp">>, now_to_timestamp(Now)},
			{<<"country">>, Country},
			{<<"joined">>, Joined}] ||  
		       #stat_invites{user=User,
				    phone=P, 
				  timestamp=Now} <-mnesia:table(stat_invites),
		       #phone_contacts{user=U2,
				       phone = P2,
				       joined=Joined} <-mnesia:table(phone_contacts),
		       User==U2 andalso P==P2,
		       #user_countries{user=U3, country=Mcc} <-mnesia:table(user_countries),
		       U3==User,
		       #mcc_country{mcc=Mcc2, country=Country} <-mnesia:table(mcc_country),
		       Mcc==Mcc2
		      ]),
	    qlc:e(Q)
    end,
    {atomic, Users} = mnesia:transaction(UserQuery),
    {atomic, Sms} = mnesia:transaction(SmsQuery),
    {atomic, File} = mnesia:transaction(FileQuery),
    {atomic, Chat} = mnesia:transaction(ChatQuery),
    {atomic, Session} = mnesia:transaction(SessionQuery),
    {atomic, Invite} = mnesia:transaction(InviteQuery),
    {stop, [{<<"users">>, Users},
	    {<<"chat">>, Chat},
	    {<<"sms">>, Sms},
	    {<<"session">>, Session},
	    {<<"invite">>, Invite},
	    {<<"file">>, File}
	   ]};
webadmin_json_api(Acc, #request{path = [<<"api">>, <<"users">>]}) -> 
    {atomic, Users} = mnesia:transaction(fun() ->
		  qlc:e(qlc:q([User || #stat_register{user = User} <- mnesia:table(stat_register)]))
	  end),
    {stop, Users};
webadmin_json_api(Acc, #request{path = [<<"api">>, <<"user_info">>, UserId]}) ->
    [#stat_register{timestamp=RegTime}] = mnesia:dirty_read(stat_register, UserId),
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,UserId),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    [#last_activity{timestamp=LastAct}] = mnesia:dirty_match_object(#last_activity{us={UserId,'_'}, _ = '_'}),
    Friends = mnesia:dirty_match_object(#roster{us={UserId,'_'}, _='_'}),
    Invites = mnesia:dirty_match_object(#stat_invites{user = UserId, _='_'}),
    Sms = mnesia:dirty_match_object(#stat_sms{user=UserId, _='_'}),
    Chat = mnesia:dirty_match_object(#stat_chat{user=UserId, _='_'}),
    Files = mnesia:dirty_match_object(#stat_file{user=UserId, _='_'}),
    
    Images = lists:filtermap(fun(#stat_file{type=Type}) ->
				     case re:run(Type, "^image*") of
					 {match,_} -> true;
					 _ -> false
				     end
			     end,
			     Files),

    {stop, [{<<"registration_date">>, now_to_timestamp(RegTime)},
	    {<<"country">>, Country},
	    {<<"sms">>, [now_to_timestamp(Time) || #stat_sms{timestamp=Time} <- Sms]},
	    {<<"messages">>, [now_to_timestamp(Time) || #stat_chat{timestamp=Time} <- Chat]},
	    {<<"total_files">>, length(Files)},
	    {<<"total_images">>, length(Images)},
	    {<<"last_activity">>, LastAct*1000},
	    {<<"contacts_count">>, length(Friends)},
	    {<<"invites_count">>, length(Invites)}]}.

register_user(User, Server) ->
    mnesia:dirty_write(#stat_register{user=User, timestamp=os:timestamp()}),
    %"hack" to avoid left joins
    mnesia:dirty_write(#last_activity{us={User,Server}, 
				      timestamp=now_to_timestamp(os:timestamp()) div 1000, 
				      status= <<"registered">>}),
    ok.

user_sent_sms(From, _To) ->
    mnesia:dirty_write(#stat_sms{user=From, timestamp=os:timestamp()}),
    ok.
user_sent_invite(From, To) ->
    mnesia:dirty_write(#stat_invites{user=From, phone = To, timestamp=os:timestamp()}),
    ok.

session_close(SID) -> 
    [#session{sid={Timestamp,_}, us={User,_}}] = mnesia:dirty_read(session,SID),
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    Length = timer:now_diff(os:timestamp(),Timestamp),
    mnesia:dirty_write(#stat_session{user=User, country=Country, start=Timestamp, length=Length}),
    ok.

http_upload_slot_request(Acc, #jid{user=User} ,_,Size,ContentType) -> 
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    mnesia:dirty_write(#stat_file{
	user=User,
	size=Size,
	country=Country,
	timestamp=os:timestamp(),
	type=ContentType}),
    Acc. 

user_send_packet(Packet,_,#jid{user=User},_To) ->
    case fxml:get_tag_attr_s(<<"type">>,Packet) of
	<<"chat">> -> 
	    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
	    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
	    mnesia:dirty_write(#stat_chat{
		user=User,
		country=Country,
		timestamp=os:timestamp()});
	_ -> ok
    end, 
    Packet.

init_mcc_table(FilePath) ->
    {ok, XmlString} = file:read_file(FilePath),
    #xmlel{children=Carriers} = fxml_stream:parse_element(XmlString),

    MccCountry = lists:filtermap(fun (X) ->
					 case X of 
					     #xmlel{} ->
						 CountryBin = fxml:get_path_s(X, [{elem, <<"country">>}, cdata]),
						 TrimmedCountryStr = re:replace(binary_to_list(CountryBin),
									       "(^\\s+)|(\\s+$)",
									       "", 
									       [global,{return,list}]),
						 Mcc = fxml:get_path_s(X, [{elem, <<"mcc">>}, cdata]),
						 {true,{list_to_binary(TrimmedCountryStr),Mcc}};
					     _ -> false
					 end
				 end,
				 Carriers),
    UniqMccCountry = sets:to_list(sets:from_list(MccCountry)),
    lists:foreach(fun({Country,Mcc}) ->
			  mnesia:dirty_write(#mcc_country{mcc=Mcc, country=Country})
		  end,
		  UniqMccCountry),
    ok.


%%%================================
%% generate test data for dashboard

remove_stat_data() ->
    mnesia:clear_table(stat_sms),
    mnesia:clear_table(stat_register),
    mnesia:clear_table(stat_session),
    mnesia:clear_table(stat_chat),
    mnesia:clear_table(stat_file).

generate_stat_data() ->
    add_stat_users(100),
    add_stat_sms(200),
    add_stat_session(200),
    add_stat_chat(400),
    add_stat_invites(100),
    add_stat_files(200).

random_mcc() ->
    Keys = mnesia:dirty_all_keys(mcc_country),
    lists:nth(random:uniform(length(Keys)),Keys).

add_stat_users(0) -> ok;
add_stat_users(Num) ->
    User = list_to_binary([random:uniform(9)+48 || _ <- lists:seq(1,11)]),
    Mcc = random_mcc(),
    SecInMonth = 31*24*60*60,
    {MSec, Sec, _} = os:timestamp(),
    Tmp = MSec*1000000+Sec-random:uniform(SecInMonth),
    RegDate = {Tmp div 1000000, Tmp rem 1000000, 0},
    mnesia:dirty_write(#user_countries{user=User, country=Mcc}),
    mnesia:dirty_write(#stat_register{user=User, timestamp=RegDate}),
    mnesia:dirty_write(#last_activity{us={User,<<"localhost">>}, 
				      timestamp=now_to_timestamp(RegDate) div 1000, 
				      status= <<"registered">>}),
    add_stat_users(Num-1).

random_stat_user() ->
    Keys = mnesia:dirty_all_keys(stat_register),
    lists:nth(random:uniform(length(Keys)),Keys).

add_stat_sms(0) -> ok;
add_stat_sms(Num) ->
    User = random_stat_user(),
    {MSec, Sec, _} = os:timestamp(),
    [#stat_register{timestamp={MSec1, Sec1, _}}] = mnesia:dirty_read(stat_register, User),
    Period = (MSec-MSec1)*1000000 + (Sec-Sec1),
    Tmp = MSec*1000000+Sec-random:uniform(Period),
    Ts = {Tmp div 1000000, Tmp rem 1000000, 0},

    mnesia:dirty_write(#stat_sms{
			  user=User,
			  timestamp = Ts}),
    add_stat_sms(Num-1).

add_stat_files(0) -> ok;
add_stat_files(Num) ->
    Types = [<<"image/png">>, <<"audio/mpeg">>, <<"application/pdf">>, <<"video/mp4">>],
    User = random_stat_user(),
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    {MSec, Sec, _} = os:timestamp(),
    [#stat_register{timestamp={MSec1, Sec1, _}}] = mnesia:dirty_read(stat_register, User),
    Period = (MSec-MSec1)*1000000 + (Sec-Sec1),
    Tmp = MSec*1000000+Sec-random:uniform(Period),
    Ts = {Tmp div 1000000, Tmp rem 1000000, 0},
    Type = lists:nth(random:uniform(4), Types),
    Size = random:uniform(2000000),
    mnesia:dirty_write(#stat_file{
			  user=User,
			  country=Country,
			  size=Size,
			  timestamp=Ts,
			  type=Type}),
    add_stat_files(Num-1).

add_stat_chat(0) -> ok;
add_stat_chat(Num) ->
    User = random_stat_user(),
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    {MSec, Sec, _} = os:timestamp(),
    [#stat_register{timestamp={MSec1, Sec1, _}}] = mnesia:dirty_read(stat_register, User),
    Period = (MSec-MSec1)*1000000 + (Sec-Sec1),
    Tmp = MSec*1000000+Sec-random:uniform(Period),
    Ts = {Tmp div 1000000, Tmp rem 1000000, 0},

    mnesia:dirty_write(#stat_chat{
			  user=User,
			  country=Country,
			  timestamp = Ts}),
    add_stat_chat(Num-1).


add_stat_session(0) -> ok;
add_stat_session(Num) ->
    User = random_stat_user(),
    [#user_countries{country=Mcc}] = mnesia:dirty_read(user_countries,User),
    [#mcc_country{country=Country}] = mnesia:dirty_read(mcc_country, Mcc),
    {MSec, Sec, _} = os:timestamp(),
    [#stat_register{timestamp={MSec1, Sec1, _}}] = mnesia:dirty_read(stat_register, User),
    Period = (MSec-MSec1)*1000000 + (Sec-Sec1),
    Tmp = MSec*1000000+Sec-random:uniform(Period),
    Ts = {Tmp div 1000000, Tmp rem 1000000, 0},
    Length = random:uniform(300000000),

    mnesia:dirty_write(#stat_session{
			  user=User,
			  country=Country,
			  length=Length,
			  start = Ts}),
    add_stat_session(Num-1).

add_stat_invites(0) -> ok;
add_stat_invites(Num) ->
    User = random_stat_user(),
    Invitee = list_to_binary([random:uniform(9)+48 || _ <- lists:seq(1,11)]),
    {MSec, Sec, _} = os:timestamp(),
    [#stat_register{timestamp={MSec1, Sec1, _}}] = mnesia:dirty_read(stat_register, User),
    Period = (MSec-MSec1)*1000000 + (Sec-Sec1),
    Tmp = MSec*1000000+Sec-random:uniform(Period),
    Ts = {Tmp div 1000000, Tmp rem 1000000, 0},
    Joined = lists:nth(random:uniform(2), [<<"true">>, <<"false">>]),

    mnesia:dirty_write(#phone_contacts{
			  user=User,
			  phone=Invitee,
			  bare_phone=Invitee,
			  joined=Joined}),
    mnesia:dirty_write(#stat_invites{user = User, phone = Invitee, timestamp = Ts}),
    add_stat_invites(Num-1).


