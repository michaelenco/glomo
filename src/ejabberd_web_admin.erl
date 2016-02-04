-module(ejabberd_web_admin).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([process/2, 
	 pretty_print_xml/1,
	 term_to_id/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-define(INPUTATTRS(Type, Name, Value, Attrs),
	?XA(<<"input">>,
	    (Attrs ++
	       [{<<"type">>, Type}, {<<"name">>, Name},
		{<<"value">>, Value}]))).

%%%==================================
%%%% get_acl_access

get_acl_rule(_RPath, _) -> {global, [configure]}.

is_acl_match(Host, Rules, Jid) ->
    lists:any(fun (Rule) ->
		      allow == acl:match_rule(Host, Rule, Jid)
	      end,
	      Rules).

%%%==================================
%%%% Menu Items Access

get_jid(Auth, HostHTTP, Method) ->
    case get_auth_admin(Auth, HostHTTP, [], Method) of
      {ok, {User, Server}} ->
	  jid:make(User, Server, <<"">>);
      {unauthorized, Error} ->
	  ?ERROR_MSG("Unauthorized ~p: ~p", [Auth, Error]),
	  throw({unauthorized, Auth})
    end.

%%%==================================
%%%% process/2

process([<<"doc">>, LocalFile], _Request) ->
    DocPath = case os:getenv("EJABBERD_DOC_PATH") of
		P when is_list(P) -> P;
		false -> <<"/share/doc/ejabberd/">>
	      end,
    FileName = filename:join(DocPath, LocalFile),
    case file:read_file(FileName) of
      {ok, FileContents} ->
	  ?DEBUG("Delivering content.", []),
	  {200, [{<<"Server">>, <<"ejabberd">>}], FileContents};
      {error, Error} ->
	  Help = <<" ", FileName/binary,
		   " - Try to specify the path to ejabberd "
		   "documentation with the environment variable "
		   "EJABBERD_DOC_PATH. Check the ejabberd "
		   "Guide for more information.">>,
	  ?INFO_MSG("Problem '~p' accessing the local Guide file ~s", [Error, Help]),
	  case Error of
	    eacces -> {403, [], <<"Forbidden", Help/binary>>};
	    enoent -> {307, [{<<"Location">>, <<"http://docs.ejabberd.im/admin/guide/configuration/">>}], <<"Not found", Help/binary>>};
	    _Else ->
		{404, [], <<(iolist_to_binary(atom_to_list(Error)))/binary, Help/binary>>}
	  end
    end;

process([<<"server">>, SHost | RPath] = Path,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    Host = jid:nameprep(SHost),
    case lists:member(Host, ?MYHOSTS) of
      true ->
	  case get_auth_admin(Auth, HostHTTP, Path, Method) of
	    {ok, {User, Server}} ->
		AJID = get_jid(Auth, HostHTTP, Method),
		process_admin(Host,
			      Request#request{path = RPath,
					      auth = {auth_jid, Auth, AJID},
					      us = {User, Server}});
	    {unauthorized, <<"no-auth-provided">>} ->
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"ejabberd\"">>}],
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       <<"Unauthorized">>)])};
	    {unauthorized, Error} ->
		{BadUser, _BadPass} = Auth,
		{IPT, _Port} = Request#request.ip,
		IPS = ejabberd_config:may_hide_data(jlib:ip_to_list(IPT)),
		?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
			     [BadUser, IPS, Error]),
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"auth error, retry login "
		     "to ejabberd\"">>}],
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       <<"Unauthorized">>)])}
	  end;
      false -> ejabberd_web:error(not_found)
    end;
process(RPath,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    case get_auth_admin(Auth, HostHTTP, RPath, Method) of
      {ok, {User, Server}} ->
	  AJID = get_jid(Auth, HostHTTP, Method),
	  process_admin(global,
			Request#request{path = RPath,
					auth = {auth_jid, Auth, AJID},
					us = {User, Server}});
      {unauthorized, <<"no-auth-provided">>} ->
	  {401,
	   [{<<"WWW-Authenticate">>,
	     <<"basic realm=\"ejabberd\"">>}],
	   ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					 <<"Unauthorized">>)])};
      {unauthorized, Error} ->
	  {BadUser, _BadPass} = Auth,
	  {IPT, _Port} = Request#request.ip,
	  IPS = ejabberd_config:may_hide_data(jlib:ip_to_list(IPT)),
	  ?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
		       [BadUser, IPS, Error]),
	  {401,
	   [{<<"WWW-Authenticate">>,
	     <<"basic realm=\"auth error, retry login "
	       "to ejabberd\"">>}],
	   ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					 <<"Unauthorized">>)])}
    end.

get_auth_admin(Auth, HostHTTP, RPath, Method) ->
    case Auth of
      {SJID, Pass} ->
	  {HostOfRule, AccessRule} = get_acl_rule(RPath, Method),
	  case jid:from_string(SJID) of
	    error -> {unauthorized, <<"badformed-jid">>};
	    #jid{user = <<"">>, server = User} ->
		get_auth_account(HostOfRule, AccessRule, User, HostHTTP,
				 Pass);
	    #jid{user = User, server = Server} ->
		get_auth_account(HostOfRule, AccessRule, User, Server,
				 Pass)
	  end;
      undefined -> {unauthorized, <<"no-auth-provided">>}
    end.

get_auth_account(HostOfRule, AccessRule, User, Server,
		 Pass) ->
    case ejabberd_auth:check_password(User, Server, Pass) of
      true ->
	  case is_acl_match(HostOfRule, AccessRule,
			    jid:make(User, Server, <<"">>))
	      of
	    false -> {unauthorized, <<"unprivileged-account">>};
	    true -> {ok, {User, Server}}
	  end;
      false ->
	  case ejabberd_auth:is_user_exists(User, Server) of
	    true -> {unauthorized, <<"bad-password">>};
	    false -> {unauthorized, <<"inexistent-account">>}
	  end
    end.

process_admin(_Host, #request{path = []}) ->
    case file:read_file("web_admin/stat.html") of
	{ok, Content} ->
	    {200, [], Content};
	Res ->
	   io:format("~n~p",[Res]), 
	    {404, [], "Not found"}
    end;

%%%=================================
%%%% json api endpoint
process_admin(_Host, #request{path=[<<"api">> | _]} = Request) ->
    case ejabberd_hooks:run_fold(webadmin_json_api, [], [Request]) of
	[] -> {404, [], "Not found"};
	Res -> {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Res)}
    end;

%%%=================================
%%%% direct files
process_admin(_Host, #request{path = Path}) ->
    P =  string:join(["web_admin"] ++ [binary_to_list(X) || X<-Path], "/"),
    io:format("~n~p",[P]),
    case file:read_file(P) of
	{ok, Content} ->
	    {200, [], Content};
	Res ->
	   io:format("~n~p",[Res]), 
	    {404, [], "Not found"}
    end.

term_to_id(T) -> jlib:encode_base64((term_to_binary(T))).

pretty_print_xml(El) ->
    list_to_binary(pretty_print_xml(El, <<"">>)).

pretty_print_xml({xmlcdata, CData}, Prefix) ->
    IsBlankCData = lists:all(
                     fun($\f) -> true;
                        ($\r) -> true;
                        ($\n) -> true;
                        ($\t) -> true;
                        ($\v) -> true;
                        ($ ) -> true;
                        (_) -> false
                     end, binary_to_list(CData)),
    if IsBlankCData ->
            [];
       true ->
            [Prefix, CData, $\n]
    end;
pretty_print_xml(#xmlel{name = Name, attrs = Attrs,
			children = Els},
		 Prefix) ->
    [Prefix, $<, Name,
     case Attrs of
       [] -> [];
       [{Attr, Val} | RestAttrs] ->
	   AttrPrefix = [Prefix,
			 str:copies(<<" ">>, byte_size(Name) + 2)],
	   [$\s, Attr, $=, $', xml:crypt(Val) | [$',
                                                 lists:map(fun ({Attr1,
                                                                 Val1}) ->
                                                                   [$\n,
                                                                    AttrPrefix,
                                                                    Attr1, $=,
                                                                    $',
                                                                    xml:crypt(Val1),
                                                                    $']
                                                           end,
                                                           RestAttrs)]]
     end,
     if Els == [] -> <<"/>\n">>;
	true ->
	    OnlyCData = lists:all(fun ({xmlcdata, _}) -> true;
				      (#xmlel{}) -> false
				  end,
				  Els),
	    if OnlyCData ->
		   [$>, xml:get_cdata(Els), $<, $/, Name, $>, $\n];
	       true ->
		   [$>, $\n,
		    lists:map(fun (E) ->
				      pretty_print_xml(E, [Prefix, <<"  ">>])
			      end,
			      Els),
		    Prefix, $<, $/, Name, $>, $\n]
	    end
     end].

opt_type(access) -> fun (V) -> V end;
opt_type(_) -> [access].
