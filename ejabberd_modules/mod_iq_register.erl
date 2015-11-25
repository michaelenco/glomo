-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:reg123">>).

-export([start/2, 
	 stop/1, 
	 stream_feature_register/2, 
	 unauthenticated_iq_register/4,
	 process_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").



start(Host, Opts) ->
	io:format("starting iq registration module!~n~n~n~n~n~n",[]),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REG, ?MODULE, process_local_iq, parallel),
	%ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE, stream_feature_register,50),
	ejabberd_hooks:add(c2s_unauthenticated_iq, Host, ?MODULE, unauthenticated_iq_register, 50),
	ok.

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_REG).
%ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,?MODULE, unauthenticated_iq_register, 50).

stream_feature_register(Acc, _Host) -> 
	io:format("stream_feature_register handler! ~n~n~n~n~n~n",[]),
	[#xmlel{name = <<"iq_register">>,
		attrs = [{<<"xmlns">>, ?NS_REG}],
		children = []}
	 | Acc].

process_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	io:format("process_iq! ~n~n~n~n~n~n",[]),
	IQ#iq{type = result,
	      sub_el =
	      [#xmlel{name = <<"query">>,
		      attrs =
		      [{<<"xmlns">>, ?NS_REG},
		       {<<"seconds">>,
			"Hello"}],
		      children = []}]}.

unauthenticated_iq_register(Acc, Server, #iq{xmlns = ?NS_REG} = IQ, IP) -> 
	io:format("unauthenticated_iq_register! ~n~n~n~n~n~n",[]),
	#xmlel{name = <<"iq_register">>,
	       attrs = [{<<"message">>, "Fuck you erlnag"}],
	       children = []};
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
	Acc.

