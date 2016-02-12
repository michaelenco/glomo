-module(mod_number_lookup).
-behaviour(gen_mod).
-export([start/2, stop/1, check_phone/1,format_phone/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(SMS_BASE_URL, "https://rosy-api.mitto.ch/nl.xml?UserName=admin2&Password=2qyaP28b&PhoneNumber=").
start(Host, Opts) ->
	ok.

stop(Host) ->
	ok.

check_phone(Phone) ->
	CleanPhone = format_phone(Phone),
	UrlPhone = binary:bin_to_list(CleanPhone),
	case httpc:request(?SMS_BASE_URL++UrlPhone) of
		{ok,Result} ->
			{_,_,Body} = Result,
			{XMLBody,_} = xmerl_scan:string(Body),
			[{xmlText, _, _, _, Ip, _}] = xmerl_xpath:string("//ip/text()", XMLBody),
			[{xmlText, _, _, _, Iv, _}] = xmerl_xpath:string("//iv/text()", XMLBody),
			if 
				( Iv /= true) ->
					error;
				true->
					[{xmlText, _, _, _, Mcc, _}] = xmerl_xpath:string("//mcc/text()", XMLBody),
					[{xmlText, _, _, _, Mnc, _}] = xmerl_xpath:string("//mnc/text()", XMLBody),
					[{ip,val(Ip)},{iv,val(Iv)},{mcc,Mcc},{mnc,Mnc}]
			end;
		_ ->
			error
	end.

format_phone(Phone) ->
	StringPhone =  binary:bin_to_list(Phone),
	Formatted = re:replace(StringPhone, "[^0-9]", "", [global, {return, list}]),
	AlbanianLookup = replace_left(Formatted,$0,"355"),
	Final = replace_left(AlbanianLookup,$8,"7"),
	list_to_binary(Final).

val(X) ->
	case X of
		"true" ->
			true;
		"false" ->
			false
	end.

replace_left(Phone,L,R) ->
	Stripped =  string:strip(Phone,left,L),
	if 
		(Stripped /= Phone) ->
			string:concat(R,Stripped);
		true ->
			Phone
	end.