-module(mod_iq_register).

-behaviour(ejabberd_config).

-behaviour(gen_mod).

-define(NS_REG, <<"jabber:iq:reg123">>).

-export([start/2, stop/1, process_local_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").

-record(last_activity, {us = {<<"">>, <<"">>} :: {binary(), binary()},
                        timestamp = 0 :: non_neg_integer(),
                        status = <<"">> :: binary()}).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_REG, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_REG).



process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Sec = 10,
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs =
				[{<<"xmlns">>, ?NS_REG},
				 {<<"seconds">>,
				  "Hello"}],
			    children = []}]}
    end.