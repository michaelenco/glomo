-module(mod_sms).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(SMS_BASE_URL, "http://10.20.254.4:13002/cgi-bin/sendsms?username=kanneluser&password=kannelpass&smsc=mitto&from=Glomo.im&to=").
-define(NS_SMS_QUOTA, <<"jabber:iq:sms_quota">>). 

-export([start/2,
         stop/1]).

-export([filter_packet/1,
	add_user_quota/2,
	get_user_quota/1,
	set_user_quota/2,
	increment_user_quota/1,
	decrement_user_quota/1,
	sms_quota_iq/3,
	send_sms/2
	]).

-record(user_sms_quota,{user= <<"">> : binary(), quota = 0  :: integer()}).

start(Host, _Opts) ->
	mnesia:create_table(user_sms_quota,
		[{disc_copies, [node()]},
		 {attributes,
		  record_info(fields, user_sms_quota)}]),
   	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50),
   	gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_SMS_QUOTA, ?MODULE, sms_quota_iq, one_queue).

stop(Host) ->
	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50),
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_SMS_QUOTA).

sms_quota_iq(From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
    	case Type of
      		set ->
	  		IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      		get ->
      			{ok,Quota} = get_user_quota(From#jid.luser),
      			QuotaResult = list_to_binary(integer_to_list(Quota)),
		  	IQ#iq{type = result,
				sub_el =
				    	[#xmlel{name = <<"query">>,
					    	attrs =
							[{<<"xmlns">>, ?NS_SMS_QUOTA},
						 	{<<"quota">>,QuotaResult}],
					    children = []}]}
    end.


filter_packet(P) ->
	{From,To,Message} = P,
	{xmlel, Name, Attrs, _Els} = Message,
	if 
		Name == <<"message">> ->
			Type = fxml:get_tag_attr_s(list_to_binary("type"), Message),
			if 
				Type == <<"sms">> ->
					SendFrom = From#jid.luser,
					{ok,UserQuota} = get_user_quota(SendFrom),
					if 
						UserQuota > 0 ->
							SendTo = To#jid.luser,
							Body = fxml:get_subtag(Message, <<"body">>),
							Text = unicode:characters_to_list(fxml:get_tag_cdata(Body)),
							send_sms(binary_to_list(SendTo),Text),
							decrement_user_quota(SendFrom),
							ejabberd_hooks:run(sms_sent,[SendFrom,SendTo]);
						true -> 
							% TODO: send error user to message, mb from node, look for this ways
							ok
					end;
				true -> ok
			end;
		true -> ok
	end,
	P.
send_sms(To,Text) ->
	case helpers:return_text_format(Text) of
		unicode ->
			Url = helpers:make_glomo_url(Text),
			SmsUrl = ?SMS_BASE_URL ++ To ++ "&text=" ++ Url ++ "&coding=2",
			{ok, {{Version, 202, ReasonPhrase}, Headers, Body}} = httpc:request(SmsUrl);
		norm ->
			Url = mod_uri:encode_uri_component(Text),
			SmsUrl = ?SMS_BASE_URL ++ To ++ "&text=" ++ Url,
			{ok, {{Version, 202, ReasonPhrase}, Headers, Body}} = httpc:request(SmsUrl)
	end.

get_user_quota(User) ->
	case mnesia:dirty_read(user_sms_quota, User) of
   		[Result] ->
   			#user_sms_quota{user = User,quota = Quota} = Result, 
			{ok, Quota};
		[] ->
	        		{ok, 0}
	end.

set_user_quota(User,Count) ->
	ok = mnesia:dirty_write(#user_sms_quota{user = User, quota = Count}).

add_user_quota(User,Count) ->
	case get_user_quota(User) of
		{ok,Quota} ->
			NewQuota = Quota + Count,
			set_user_quota(User,NewQuota);
		{error,notfound} ->
			set_user_quota(User,Count);
		_ -> {error,somethingwrong}
	end.

increment_user_quota(User) ->
	add_user_quota(User,1).

decrement_user_quota(User)->
	add_user_quota(User,-1).