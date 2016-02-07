-module(mod_stat).

-behaviour(gen_mod).

-export([start/2, stop/1, 
	 register_user/2, 
	 init_mcc_table/1, 
	 user_sent_sms/2, 
	 close_session/4, 
	 http_upload_slot_request/5,
	 webadmin_json_api/2,
	 user_send_packet/4,
	 generate_stat_data/0,
	 remove_stat_data/0]).  

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).
-record(stat_register, {user, timestamp}).
-record(mcc_country, {mcc, country}).
-record(stat_sms, {user, timestamp}).
-record(stat_sessions, {user, start, length}).
-record(stat_file, {user, size, type, timestamp, country}).
-record(stat_chat, {user, timestamp, country}).

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
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, stat_file)}]),
    mnesia:create_table(stat_chat, 
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stat_chat)}]),

    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 50),
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:add(http_upload_slot_request, Host, ?MODULE, http_upload_slot_request, 50),
    ejabberd_hooks:add(sms_sent, Host, ?MODULE, user_sent_sms, 50),
    ejabberd_hooks:add(close_session, Host, ?MODULE, close_session, 50),
    ejabberd_hooks:add(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 50),
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:delete(http_upload_slot_request, Host, ?MODULE, http_upload_slot_request, 50),
    ejabberd_hooks:delete(close_session, Host, ?MODULE, close_session, 50),
    ejabberd_hooks:delete(sms_sent, Host, ?MODULE, user_sent_sms, 50),

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

    {atomic, Users} = mnesia:transaction(UserQuery),
    {atomic, Sms} = mnesia:transaction(SmsQuery),
    {atomic, File} = mnesia:transaction(FileQuery),
    {atomic, Chat} = mnesia:transaction(ChatQuery),
    {stop, [{<<"users">>, Users},
	    {<<"chat">>, Chat},
	    {<<"sms">>, Sms},
	    {<<"file">>, File}
	   ]}.

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

close_session({SessionStart, _}, #jid{user=User, server=Server},_, _) ->
    io:format("session closed ~n"),
    mnesia:dirty_write(#stat_sessions{user={User, Server}, start=SessionStart, length=time:now_diff(os:timestamp(), SessionStart)}),
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
    case xml:get_tag_attr_s(<<"type">>,Packet) of
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
    #xmlel{children=Carriers} = xml_stream:parse_element(XmlString),

    MccCountry = lists:filtermap(fun (X) ->
					 case X of 
					     #xmlel{} ->
						 CountryBin = xml:get_path_s(X, [{elem, <<"country">>}, cdata]),
						 TrimmedCountryStr = re:replace(binary_to_list(CountryBin),
									       "(^\\s+)|(\\s+$)",
									       "", 
									       [global,{return,list}]),
						 Mcc = xml:get_path_s(X, [{elem, <<"mcc">>}, cdata]),
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
    mnesia:clear_table(stat_chat),
    mnesia:clear_table(stat_file).

generate_stat_data() ->
    add_stat_users(100),
    add_stat_sms(200),
    add_stat_chat(400),
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
			  user=random_stat_user(),
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
			  user=random_stat_user(),
			  country=Country,
			  timestamp = Ts}),
    add_stat_chat(Num-1).
