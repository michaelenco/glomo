-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:reg123">>).

-export([start/2, stop/1, process_local_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").



start(Host, Opts) ->
	gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				      ?NS_REG, ?MODULE, process_local_iq, parallel),
	ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
			   ?MODULE, unauthenticated_iq_register, 50).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
					 ?NS_REG),
	ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
			      ?MODULE, unauthenticated_iq_register, 50).



process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
	case Type of
		set ->
			IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
		get ->
			IQ#iq{type = result,
			      sub_el =
			      [#xmlel{name = <<"query">>,
				      attrs =
				      [{<<"xmlns">>, ?NS_REG},
				       {<<"seconds">>,
					"Hello"}],
				      children = []}]}
	end.

unauthenticated_iq_register(_Acc, Server,#iq{type = Type, sub_el = SubEl} = IQ) -> 
	IQ#iq{type = result,
			      sub_el =
			      [#xmlel{name = <<"query">>,
				      attrs =
				      [{<<"xmlns">>, ?NS_REG},
				       {<<"seconds">>,
					"Hello"}],
				      children = []}]}.
