-module(mod_stat).

-behaviour(gen_mod).

-export([start/2, stop/1, 
	 register_user/2, 
	 init_mcc_table/1, 
	 user_sent_sms/1, 
	 close_session/4, 
	 http_upload_slot_request/4,
	 webadmin_json_api/2,
	 init_test_stat_data/1]).

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
-record(stat_file, {user, size, type}).

-record(last_activity, {us, timestamp, status}).  

start(Host, _Opts) ->
    mnesia:create_table(mcc_country, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, mcc_country)}]),
    mnesia:create_table(stat_sms, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, stat_sms)}]),
    mnesia:create_table(stat_register, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, stat_register)}]),
    mnesia:create_table(stat_file, 
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, stat_file)}]),

    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:add(user_sent_sms, Host, ?MODULE, user_sent_sms, 50),
    ejabberd_hooks:add(close_session, Host, ?MODULE, close_session, 50),

    ejabberd_hooks:add(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:delete(close_session, Host, ?MODULE, close_session, 50),
    ejabberd_hooks:delete(user_sent_sms, Host, ?MODULE, user_sent_sms, 50),

    ejabberd_hooks:delete(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

now_to_timestamp({MegaSec, Sec, MiliSec}) ->
    MegaSec*1000000000+Sec*1000+(MiliSec div 1000).

webadmin_json_api(Acc, #request{path = [<<"api">>, <<"stat">>]}) -> 
    Fun = fun() -> 
		  Q = qlc:q([
			     [{<<"user">>,User},
			      {<<"timestamp">>,now_to_timestamp(Now)},
			      {<<"country">>, Country},
			      {<<"last_activity">>, LT*1000}
			     ] ||
			     #stat_register{user=User, timestamp=Now} <- mnesia:table(stat_register),
			     #user_countries{user=CUser, country=CMcc} <- mnesia:table(user_countries),
			     User == CUser,
			     #mcc_country{mcc=Mcc, country=Country} <- mnesia:table(mcc_country),
			     Mcc == CMcc,
			     #last_activity{us={LU,_}, timestamp=LT} <- mnesia:table(last_activity),
			     User == LU
			    ]),	
		  qlc:e(Q)
	  end,
    {atomic, Res} = mnesia:transaction(Fun),
    io:format("~nregs count: ~p",[length(Res)]),
    {stop, Res}.

register_user(User, Server) ->
    mnesia:dirty_write(#stat_register{user=User, timestamp=os:timestamp()}),
    %"hack" to avoid left joins
    mnesia:dirty_write(#last_activity{us={User,Server}, 
				      timestamp=now_to_timestamp(os:timestamp()) div 1000, 
				      status= <<"registered">>}),
    ok.

user_sent_sms({User, Server}) ->
    mnesia:dirty_write(#stat_sms{user={User,Server}, timestamp=os:timestamp()}),
    ok.

close_session({SessionStart, _}, #jid{user=User, server=Server},_, _) ->
    io:format("session closed ~n"),
    mnesia:dirty_write(#stat_sessions{user={User, Server}, start=SessionStart, length=time:now_diff(os:timestamp(), SessionStart)}),
    ok.

http_upload_slot_request(_,_,_,_) -> ok.


init_mcc_table(FilePath) ->
    {ok, XmlString} = file:read_file(FilePath),
    #xmlel{children=Carriers} = xml_stream:parse_element(XmlString),

    MccCountry = lists:filtermap(fun (X) ->
					 case X of 
					     #xmlel{} ->
						 Country = xml:get_path_s(X, [{elem, <<"country">>}, cdata]),
						 Mcc = xml:get_path_s(X, [{elem, <<"mcc">>}, cdata]),
						 {true,{Country,Mcc}};
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

random_mcc() ->
    Keys = mnesia:dirty_all_keys(mcc_country),
    lists:nth(random:uniform(length(Keys)),Keys).

init_test_stat_data(1) -> ok;
init_test_stat_data(Num) ->
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
    init_test_stat_data(Num-1).

