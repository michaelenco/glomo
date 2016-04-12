-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:register_phone">>).
-define(NS_CONFIRM, <<"jabber:iq:confirm_phone">>).
-define(DOMAIN, <<"localhost">>).
-define(SMS_BASE_URL, "http://10.20.254.4:13002/cgi-bin/sendsms?username=kanneluser&password=kannelpass&smsc=mitto&from=Glomo.im&to=").
-define(ALLOWED_CHARS, [$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z]).

-export([start/2, 
	 stop/1, 
	 unauthenticated_iq/4,
	 get_random_string/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-record(user_countries,{user= <<"">>, country = <<"">>}).

get_random_string(Length, AllowedChars) ->
	random:seed(erlang:now()),
 	lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

start(Host, Opts) ->
	mnesia:create_table(user_countries,
		[{disc_copies, [node()]},
		 {attributes,
		  record_info(fields, user_countries)}]),
	ejabberd_hooks:add(c2s_unauthenticated_iq, Host, ?MODULE, unauthenticated_iq, 50),
	inets:start(),
	ok.

stop(Host) ->
	ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,?MODULE, unauthenticated_iq_register, 50).


unauthenticated_iq(Acc, Server, #iq{xmlns = ?NS_REG, sub_el = SubEl} = IQ, IP) -> 
	PhoneTag = fxml:get_subtag(SubEl, <<"phone">>),
	if 
		(PhoneTag /= false)->

			{xmlel,_,_,PhoneChildren} = PhoneTag,
			PhoneNumber = fxml:get_cdata(PhoneChildren),
			FormattedPhone = mod_number_lookup:format_phone(PhoneNumber),
			SmsPhone = binary:bin_to_list(FormattedPhone),

			NewPasswd = list_to_binary(["1","1","1","1","1"]),
			%NewPasswd = list_to_binary(get_random_string(6,["1","2","3","4","5","6","7","8","9","0"])),

			UserExists = ejabberd_auth:is_user_exists(FormattedPhone,Server),

			if 
				(UserExists == true) ->
					ejabberd_auth:set_password(FormattedPhone,Server,NewPasswd),
					SmsUrl = ?SMS_BASE_URL ++ SmsPhone ++ "&text=Your+glomo+code:" ++ binary_to_list(NewPasswd),
					{ok, {{Version, 202, ReasonPhrase}, Headers, Body}} = httpc:request(SmsUrl),
					Jid = jlib:jid_to_string(#jid{user = FormattedPhone, server = Server}),
					jlib:iq_to_xml(IQ#iq{
						type = result,
					 	sub_el = [
						   #xmlel{
						      	name = <<"account">>, 
						      	attrs = [
							    	{<<"jid">>,Jid}
							    ] 
						     }
						  ]
					});
				true ->	 
					CheckPhone = mod_number_lookup:check_phone(FormattedPhone),
					case (CheckPhone) of
						[{ip,Ip},{iv,Iv},{mcc,Mcc},{mnc,Mnc}] ->
							if
								(Iv == true) ->
									{atomic, ok} = ejabberd_auth:try_register(FormattedPhone, Server, NewPasswd),
									SmsUrl = ?SMS_BASE_URL ++ SmsPhone ++ "&text=Your+glomo+code:" ++ binary_to_list(NewPasswd),
									{ok, {{Version, 202, ReasonPhrase}, Headers, Body}} = httpc:request(SmsUrl),
									ok = mnesia:dirty_write(#user_countries{user = FormattedPhone, country = list_to_binary(Mcc)}),
									Jid = jlib:jid_to_string(#jid{user = FormattedPhone, server = Server}),
									jlib:iq_to_xml(IQ#iq{
									 	type = result,
										sub_el = [
										   #xmlel{
										      	name = <<"account">>, 
										      	attrs = [
											       {<<"jid">>,Jid}
											    ] 
									     	}
								    	]
									});
								true ->
									jlib:iq_to_xml(IQ#iq{
										type = error,
									 	sub_el = [
										    #xmlel{
										      	name = <<"error">>, 
										      	attrs = [
											    	{<<"reason">>,<<"not valid phone number">>}
											    ] 
										    }
										]
									})
							end;
						error ->
							jlib:iq_to_xml(IQ#iq{
								type = error,
							 	sub_el = [
								    #xmlel{
								      	name = <<"error">>, 
								      	attrs = [
									    	{<<"reason">>,<<"not valid phone number">>}
									    ] 
								    }
								]
							})
					end
			end;
		true -> 
			jlib:iq_to_xml(IQ#iq{type = error,sub_el = [SubEl, ?ERR_NOT_ALLOWED]})
	end;

unauthenticated_iq(Acc, _Server, _IQ, _IP) ->
	Acc.

