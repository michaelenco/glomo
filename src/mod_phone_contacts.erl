-module(mod_phone_contacts).

%-behaviour(ejabberd_config).
%
%TODO:	find and remove jid from table on contacts update
%

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,get_contact_phones/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_PHONE_CONTACTS, <<"jabber:iq:phone_contacts">>). 

-record(phone_contacts,{user= <<"">>, phone = <<"">>, bare_phone = <<"">>, joined = <<"">>}).
-record(vcard, {us = {<<"">>, <<"">>} :: {binary(), binary()} | binary(),
                vcard = #xmlel{} :: xmlel()}).


start(Host, _Opts) ->
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

%Setting phone contacts into base
process_local_iq(From, _To,
		     #iq{type = set, sub_el = SubEl} = IQ) ->
    Items = fxml:get_subtags(SubEl, <<"item">>),
    mnesia:dirty_delete(phone_contacts,jlib:jid_to_string(jlib:jid_remove_resource(From))),
    Count = set_contact_phones(jlib:jid_remove_resource(From),Items,0),
    XCount = list_to_binary(integer_to_list(Count)),
    Children = [#xmlel{name = <<"succes">>,attrs = [{<<"count">>,XCount}]}],
    IQ#iq{type = result,
    sub_el =
            [#xmlel{name = <<"query">>,
                attrs =
                [{<<"xmlns">>, ?NS_PHONE_CONTACTS}],
            children = Children}]};

%Getting phone contacts from base
process_local_iq(From, _To,
             #iq{type = get, sub_el = SubEl} = IQ) ->
    XMLItems = get_contact_phones(jlib:jid_to_string(jlib:jid_remove_resource(From))),
    IQ#iq{type = result,
    sub_el =
            [#xmlel{name = <<"query">>,
                attrs =
                [{<<"xmlns">>, ?NS_PHONE_CONTACTS}],
            children = XMLItems}]}.

set_contact_phones(From, [], Res) ->
    Res;

set_contact_phones(From, [#xmlel{attrs = Attrs}|T], Res) ->
    {value,PhoneNumber} = fxml:get_attr(<<"phone">>,Attrs),
    FormattedNumber = mod_number_lookup:format_phone(PhoneNumber),    
    case ejabberd_auth:is_user_exists(FormattedNumber,From#jid.lserver) of 
	true->
                Flag = <<"true">>,
                mnesia:dirty_write(#phone_contacts{user=jlib:jid_to_string(From), phone=FormattedNumber, bare_phone=PhoneNumber, joined=Flag});
	_ -> 
                Flag = <<"false">>,
                mnesia:dirty_write(#phone_contacts{user=jlib:jid_to_string(From), phone=FormattedNumber, bare_phone=PhoneNumber, joined=Flag})
    end,
    set_contact_phones(From, T, Res+1).

get_contact_phones(From) ->
    Records = mnesia:dirty_match_object(phone_contacts,{phone_contacts,From,'_','_','_'}),
    XMLItems = lists:foldl(fun(Element,Accum) -> make_contacts_result(Element, Accum) end, [], Records).

make_contacts_result(#phone_contacts{user=From, phone=Phone, bare_phone=Bare, joined=Joined}, Accum) ->
    if 
        (Joined == <<"true">>)->
            FromJid = jlib:string_to_jid(From),
            Jid = jlib:jid_to_string(#jid{user = Phone, server = FromJid#jid.lserver}),
            StatusMessage = get_status(Phone,FromJid#jid.lserver),
            lists:append(Accum,[#xmlel{name = <<"item">>,attrs = [{<<"phone">>,Bare},{<<"registered">>,<<"true">>},{<<"jid">>,Jid},{<<"status">>,StatusMessage}]}]);
        true ->
            case mod_invites:is_invited(From,Bare) of 
                    true->
                        lists:append(Accum,[#xmlel{name = <<"item">>,attrs = [{<<"phone">>,Bare},{<<"invited">>,<<"true">>}]}]);
                    false->
                        lists:append(Accum,[#xmlel{name = <<"item">>,attrs = [{<<"phone">>,Bare},{<<"invited">>,<<"false">>}]}])
            end
    end.

get_status(LUser,LServer) ->
        Vcard = get_vcard(LUser,LServer),
        case Vcard of
            [H|_] ->
                StatusMessage = fxml:get_path_s(H, [{elem, <<"SM">>}, cdata]),
                StatusMessage;
            [] ->
                <<>>;
            false ->
                <<>>
        end.


get_vcard(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:read({vcard, US}) end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
            lists:map(fun (R) -> R#vcard.vcard end, Rs);
      {aborted, _Reason} -> 
            error
end.