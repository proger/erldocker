-module(docker).
-compile([export_all]).
-include("erldocker.hrl").

%
% Misc
% 

% @doc Identical to the docker info command.
info() ->
    erldocker_api:get(info).

% @doc Identical to the docker version command.
version() ->
    erldocker_api:get(version).

events() ->
    % XXX: couldn't get it to work, docker 0.6.1
    erldocker_api:get_stream([events]).

%%%%% docker-py COPY-PASTE BELOW

% @doc Creates a container that can then be started.
% Parameters are similar to those for the docker run command except it doesn't support the attach options (-a)
create_container(Image, Command, [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false}, {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined}, {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}] = Args) -> {error, not_implemented}.

% @doc Similar to the docker build command.
% path (required) can be a local path (To a directory containing a Dockerfile) or a remote URL
build([{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker port command.
port(Container, PrivatePort) -> {error, not_implemented}.

%%%%%%%%%%%%%

default_args(build) ->
    [{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}];
default_args(create_container) ->
    [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false},
        {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined},
        {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}];

default_args(_) ->
    [].

