-module(mod_phone_contacts).

%-behaviour(ejabberd_config).
%
%TODO:	find and remove jid from table on contacts update
%

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_PHONE_CONTACTS, <<"jabber:iq:phone_contacts">>). 

-record(phone_contacts,{user= <<"">>, phone = <<"">>, bare_phone = <<"">>}).


start(Host, Opts) ->
    mnesia:create_table(phone_contacts,
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes,
			  record_info(fields, phone_contacts)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_PHONE_CONTACTS, ?MODULE, process_local_iq, one_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_PHONE_CONTACTS).


process_local_iq(From, _To,
		     #iq{type = set, sub_el = SubEl} = IQ) ->
    Items = xml:get_subtags(SubEl, <<"item">>),
    mnesia:dirty_delete(phone_contacts,From),
    Result = set_contact_phones(jlib:jid_remove_resource(From),Items,[]),
    XMLItems = lists:map(fun(X) -> phone_jid_map(X) end,Result),
    IQ#iq{type = result,sub_el = XMLItems}.

set_contact_phones(From, [], Res) ->
    Res;

set_contact_phones(From, [#xmlel{attrs = Attrs}|T], Res) ->
    {value,PhoneNumber} = xml:get_attr(<<"phone">>,Attrs),
    FormattedNumber = mod_number_lookup:format_phone(PhoneNumber),
    mnesia:dirty_write(#phone_contacts{user=jlib:jid_to_string(From), phone=FormattedNumber, bare_phone=PhoneNumber}),
    case ejabberd_auth:is_user_exists(FormattedNumber,From#jid.lserver) of 
	true->
	    NewRes = [{PhoneNumber,jlib:jid_to_string(#jid{user = FormattedNumber, server = From#jid.lserver})}| Res];
	_ -> NewRes = Res
    end,
    set_contact_phones(From, T, NewRes).

phone_jid_map(X) ->
    {Phone,Jid} = X,
    #xmlel{name = <<"item">>,attrs = [{<<"phone">>,Phone},{<<"jid">>,Jid}]}.
