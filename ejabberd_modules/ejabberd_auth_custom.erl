-module(ejabberd_auth_custom).

-behaviour(ejabberd_auth).
-behaviour(ejabberd_config).

-include("ejabberd.hrl").
-include("logger.hrl").

-export([start/1,
	 stop/1,get_password/2,
	 check_password/5,
	 check_password/3]
       ).

start(Host) ->
    ok.

stop(_Host) ->
    ok.

get_password(User,Domain) ->
	<<"123">>.

check_password(User,Domain,Password,Digest, DigestGen) ->
	io:format("!!!!!!!!!!!!! ~w~n",[Password]),
	io:format("!!!!!!!!!!!!! ~w~n",[User]),
	true.

check_password(User,Domain,Password) ->
	true.
