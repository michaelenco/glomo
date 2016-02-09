-module(mod_invites).

%-behaviour(ejabberd_config).
%
%TODO:	find and remove jid from table on contacts update
%

-behaviour(gen_mod).

-export([start/2, stop/1, batch_invite/3,is_invited/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_INVITES, <<"jabber:iq:glomo_invites">>). 

-record(user_invites,{user= <<"">>, phone = <<"">>, timestamp}).

start(Host, Opts) ->
    mnesia:create_table(user_invites,
			[{type,bag},
			 {disc_copies, [node()]},
			 {attributes,
			  record_info(fields, user_invites)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_INVITES, ?MODULE, batch_invite, one_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_INVITES).


batch_invite(From, _To,
		     #iq{type = set, sub_el = SubEl} = IQ) ->
    Items = fxml:get_subtags(SubEl, <<"item">>),
    Result = set_invited_phones(jlib:jid_remove_resource(From),Items),
    IQ#iq{type = result,
        sub_el =
                [#xmlel{name = <<"query">>,
                    attrs =
                    [{<<"xmlns">>, ?NS_INVITES},{<<"result">>,<<"ok">>}],
                children = []}]}.

set_invited_phones(From, []) ->
    ok;

set_invited_phones(From, [#xmlel{attrs = Attrs}|T]) ->
    {value,PhoneNumber} = fxml:get_attr(<<"phone">>,Attrs),
    FormattedNumber = mod_number_lookup:format_phone(PhoneNumber),
    Joined = <<"false">>,
    mnesia:dirty_write(#user_invites{user=jlib:jid_to_string(From), phone=FormattedNumber, timestamp=os:timestamp()}),
    mod_sms:send_sms(binary_to_list(FormattedNumber),"Holla! Let's use Glomo for chating! http://glomo.im"),
    set_invited_phones(From, T).

is_invited(From,Phone) ->
    case mnesia:dirty_match_object({user_invites,From,Phone}) of 
       [Record] ->
            true;
        [] ->
            false;
        _ -> 
            false
    end.
