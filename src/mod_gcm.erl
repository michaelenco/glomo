-module(mod_gcm).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-behaviour(gen_mod).

-record(gcm_users, {user, gcm_key}).

-export([start/2, stop/1, push/2, process_local_iq/3]).

-define(NS_PUSH_ID, <<"jabber:iq:push_id">>).

start(Host, Opts) ->
    ApiKey = gen_mod:get_opt(api_key, Opts, fun(X) -> X end),
    application:start(inets),
    application:start(jsx),
    application:start(gcm),
    gcm:start(main, binary_to_list(ApiKey)),
    mnesia:create_table(gcm_users, [{disc_copies, [node()]}, {attributes, record_info(fields, gcm_users)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUSH_ID, ?MODULE, process_local_iq, one_queue),
    ok.

stop(Host) -> 
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PUSH_ID),
    ok.

process_local_iq(From,_To,#iq{type=set, sub_el = SubEl} = IQ) -> 
    Tag = fxml:get_subtag(SubEl, <<"reg_id">>),
    {xmlel,_,_,Children} = Tag,
    RegId = fxml:get_cdata(Children),
    mnesia:dirty_write(#gcm_users{user=jlib:jid_to_string(jlib:jid_remove_resource(From)), gcm_key = RegId}),
    IQ#iq{type=result, sub_el = [SubEl]}.

push(To,Message) ->
    io:format("Sending push notification to ~s, message: ~s~n~n",[jlib:jid_to_string(jlib:jid_remove_resource(To)), Message]),
    case mnesia:dirty_read(gcm_users, jlib:jid_to_string(jlib:jid_remove_resource(To))) of
	[#gcm_users{gcm_key=Key}] ->
	    gcm:push(main, [Key], [{<<"data">>, [{<<"message">>, Message}]}]);
	_->
	    error
    end.
