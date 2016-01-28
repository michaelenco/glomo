-module(mod_stat).

-behaviour(gen_mod).

-export([start/2, stop/1, register_user/2, init_mcc_table/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).
-record(stat_reg, {user, country, timestamp}).
-record(mcc_country, {mcc, country}).

start(Host, Opts) ->
    mnesia:create_table(mcc_country, 
			[{attributes, record_info(fields, mcc_country)}]),
    mnesia:create_table(user_countries, 
			[{attributes, record_info(fields, user_countries)}]),
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 50).

register_user(User, Server) ->
    case mnesia:dirty_read(user_countries, User) of
	[#user_countries{country=Mcc}] ->
	    io:format("~n~nMcc:~p~n~n",[Mcc]),
	    ok;
	Var ->
	    io:format("~n~nno mcc set for user, ~p~n~n",[Var])
    end.

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
			  mnesia:dirty_write(#mcc_country{mcc=Mcc, country=Country}),
		  end,
		  UniqMccCountry),
    ok.

