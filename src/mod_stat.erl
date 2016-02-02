-module(mod_stat).

-behaviour(gen_mod).

-export([start/2, stop/1, 
	 register_user/2, 
	 init_mcc_table/1, 
	 user_sent_sms/1, 
	 close_session/4, 
	 http_upload_slot_request/4,
	 webadmin_menu_main/2,
	 webadmin_page_main/2,
	 webadmin_json_api/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).
-record(stat_register, {user, timestamp}).
-record(mcc_country, {mcc, country}).
-record(stat_sms, {user, timestamp}).
-record(stat_sessions, {user, start, length}).
-record(stat_file, {user, size, type}).



start(Host, Opts) ->
    mnesia:create_table(mcc_country, 
			[{attributes, record_info(fields, mcc_country)}]),
    mnesia:create_table(stat_sms, 
			[{attributes, record_info(fields, stat_sms)}]),
    mnesia:create_table(stat_register, 
			[{attributes, record_info(fields, stat_register)}]),
    mnesia:create_table(stat_file, 
			[{attributes, record_info(fields, stat_file)}]),

    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:add(user_sent_sms, Host, ?MODULE, user_sent_sms, 50),
    ejabberd_hooks:add(close_session, Host, ?MODULE, close_session, 50),

    ejabberd_hooks:add(webadmin_menu_main, ?MODULE, webadmin_menu_main, 50),
    ejabberd_hooks:add(webadmin_page_main, ?MODULE, webadmin_page_main, 50),
    ejabberd_hooks:add(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50),
    ejabberd_hooks:delete(close_session, Host, ?MODULE, close_session, 50),
    ejabberd_hooks:delete(user_sent_sms, Host, ?MODULE, user_sent_sms, 50),

    ejabberd_hooks:delete(webadmin_menu_main, ?MODULE, webadmin_menu_main, 50),
    ejabberd_hooks:delete(webadmin_page_main, ?MODULE, webadmin_page_main, 50),
    ejabberd_hooks:delete(webadmin_json_api, ?MODULE, webadmin_json_api, 50),
    ok.

webadmin_menu_main(Acc, Lang) ->
    Acc ++ [{<<"stat">>,"Statistics"}].

webadmin_page_main(_, _) ->
    {ok, Content} = file:read_file("web_admin/stat.html"),
    {stop, [xml_stream:parse_element(Content)]}.
    
webadmin_json_api(Acc, #request{path = [<<"api">>, <<"stat">>]}) -> 
    {stop, <<"hello api">>}.

register_user(User, Server) ->
    mnesia:dirty_write(#stat_register{user={User,Server}, timestamp=os:timestamp()}),
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

