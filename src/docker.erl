-module(docker).
-include("erldocker.hrl").

-export([info/0, version/0, events/0, auth/1]).

% @doc Identical to the docker info command.
info() ->
    ?PROPLIST(erldocker_api:get(info)).

% @doc Identical to the docker version command.
version() ->
    ?PROPLIST(erldocker_api:get(version)).

% @doc Returns {ok, Pid}. Starts sending messages to calling process.
events() ->
    % XXX: seems broken, docker 0.6.1
    erldocker_api:get_stream(events).

auth(JsonConfigBin) ->
    erldocker_api:post(auth, [], JsonConfigBin).
