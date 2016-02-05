-module(mod_invites).

%-behaviour(ejabberd_config).
%
%TODO:	find and remove jid from table on contacts update
%

-behaviour(gen_mod).

-export([start/2, stop/1, batch_invite/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_BATCH_INVITE, <<"jabber:iq:batch_invite">>). 

-record(user_invites,{user= <<"">>, phone = <<"">>, bare_phone = <<"">>}).

start(Host, Opts) ->
    mnesia:create_table(user_invites,
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes,
			  record_info(fields, user_invites)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_BATCH_INVITE, ?MODULE, batch_invite, one_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_BATCH_INVITE).


batch_invite(From, _To,
		     #iq{type = set, sub_el = SubEl} = IQ) ->
    Items = xml:get_subtags(SubEl, <<"item">>),
    Result = set_invited_phones(jlib:jid_remove_resource(From),Items),
    IQ#iq{type = result,
        sub_el =
                [#xmlel{name = <<"query">>,
                    attrs =
                    [{<<"xmlns">>, ?NS_BATCH_INVITE},{<<"result">>,<<"ok">>}],
                children = []}]}.

set_invited_phones(From, []) ->
    ok;

set_invited_phones(From, [#xmlel{attrs = Attrs}|T]) ->
    {value,PhoneNumber} = xml:get_attr(<<"phone">>,Attrs),
    FormattedNumber = mod_number_lookup:format_phone(PhoneNumber),
    mnesia:dirty_write(#user_invites{user=jlib:jid_to_string(From), phone=FormattedNumber, bare_phone=PhoneNumber}),
    set_invited_phones(From, T).