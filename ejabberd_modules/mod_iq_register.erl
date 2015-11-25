-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:register_phone">>).

-export([start/2, 
	 stop/1, 
	 unauthenticated_iq_register/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

start(Host, Opts) ->
	ejabberd_hooks:add(c2s_unauthenticated_iq, Host, ?MODULE, unauthenticated_iq_register, 50),
	ok.

stop(Host) ->
	ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,?MODULE, unauthenticated_iq_register, 50).


unauthenticated_iq_register(Acc, Server, #iq{xmlns = ?NS_REG, sub_el = SubEl} = IQ, IP) -> 
	PhoneTag = xml:get_subtag(SubEl, <<"phone">>),
	if
		(PhoneTag /= false) -> 
			io:format("phone tag: ~w ~n~n~n",[PhoneTag]),
			{xmlel,_,_,Children} = PhoneTag,
			PhoneNumber = xml:get_cdata(Children),
			io:format("here we have to send some sms to ~s number~n~n~n",[PhoneNumber]),
%			case ejabberd_auth:try_register(Username, Host,Password)
%			of
%				{atomic, Res} ->
%					{success, Res, {Username, Host, Password}};
%				Other -> Other
%			end;
			jlib:iq_to_xml(IQ#iq{type = result,sub_el = []});
		true ->
			jlib:iq_to_xml(IQ#iq{type = error,sub_el = [SubEl, ?ERR_NOT_ALLOWED]})
	end;
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
	Acc.

