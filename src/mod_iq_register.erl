-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:register_phone">>).
-define(NS_CONFIRM, <<"jabber:iq:confirm_phone">>).
-define(DOMAIN, <<"localhost">>).
-define(SMS_BASE_URL, "http://10.20.254.4:13002/cgi-bin/sendsms?username=kanneluser&password=kannelpass&smsc=mitto&from=test&to=").
-define(ALLOWED_CHARS, [$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z]).

-export([start/2, 
	 stop/1, 
	 unauthenticated_iq/4]).

-export([random_password/1, random_char/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-record(sms_passwd, {phone = <<"">> :: binary() ,passwd = <<"">> :: binary()} ).

random_char() ->
	Val = rand:uniform(length(?ALLOWED_CHARS)),
	lists:nth(Val, ?ALLOWED_CHARS).

random_password(0) -> 
	[];
random_password(N) ->
	[random_char()|random_password(N-1)].

sms_passwd_schema() ->
	{record_info(fields, sms_passwd), #sms_passwd{}}.

start(Host, Opts) ->
	mnesia:create_table(sms_passwd,[{attributes, record_info(fields, sms_passwd)}]),
	ejabberd_hooks:add(c2s_unauthenticated_iq, Host, ?MODULE, unauthenticated_iq, 50),
	inets:start(),
	ok.

stop(Host) ->
	ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,?MODULE, unauthenticated_iq_register, 50).


unauthenticated_iq(Acc, Server, #iq{xmlns = ?NS_REG, sub_el = SubEl} = IQ, IP) -> 
	PhoneTag = xml:get_subtag(SubEl, <<"phone">>),
	if 
		(PhoneTag /= false)->

			{xmlel,_,_,PhoneChildren} = PhoneTag,
			PhoneNumber = xml:get_cdata(PhoneChildren),

			NewPasswd = list_to_binary(random_password(10)),

			{atomic, ok} = ejabberd_auth:try_register(PhoneNumber, Server, NewPasswd),
			jlib:iq_to_xml(IQ#iq{
					 type = result,
					 sub_el = [
						   #xmlel{
						      name = <<"new-account">>, 
						      attrs = [
							       {<<"jid">>, jlib:jid_to_string(#jid{ user = PhoneNumber, server = Server})},
							       {<<"password">>, NewPasswd}
							      ] 
						     }
						  ]
					});
		true -> 
			jlib:iq_to_xml(IQ#iq{type = error,sub_el = [SubEl, ?ERR_NOT_ALLOWED]})
	end;

unauthenticated_iq(Acc, Server, #iq{xmlns = ?NS_CONFIRM, sub_el = SubEl} = IQ, IP) ->
	PasswdTag = xml:get_subtag(SubEl, <<"password">>),
	PhoneTag = xml:get_subtag(SubEl, <<"phone">>),
	if 
		(PhoneTag /= false) and (PasswdTag /= false) ->

			{xmlel,_,_,PhoneChildren} = PhoneTag,
			PhoneNumber = xml:get_cdata(PhoneChildren),

			{xmlel,_,_,PasswdChildren} = PasswdTag,
			Passwd = xml:get_cdata(PasswdChildren),

			case mnesia:dirty_read(sms_passwd,  PhoneNumber) of
				[#sms_passwd{passwd = Passwd}] -> 
					NewPasswd = list_to_binary(random_password(10)),

					{atomic, ok} = ejabberd_auth:try_register(PhoneNumber, ?DOMAIN, NewPasswd),
					jlib:iq_to_xml(IQ#iq{
							 type = result,
							 sub_el = [
								   #xmlel{
								      name = <<"new_password">>, 
								      attrs = [], 
								      children = [{xmlcdata, NewPasswd}]
								     }
								  ]
							});
				_ ->
					jlib:iq_to_xml(IQ#iq{type = error ,sub_el = []})
			end;				
		true -> 
			jlib:iq_to_xml(IQ#iq{type = error,sub_el = [SubEl, ?ERR_NOT_ALLOWED]})
	end;

unauthenticated_iq(Acc, _Server, _IQ, _IP) ->
	Acc.

