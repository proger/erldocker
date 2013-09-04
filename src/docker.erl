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
    erldocker_api:post_stream([events]).

%%%%% docker-py COPY-PASTE BELOW

% @doc Creates a container that can then be started.
% Parameters are similar to those for the docker run command except it doesn't support the attach options (-a)
create_container(Image, Command, [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false}, {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined}, {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}] = Args) -> {error, not_implemented}.

% @doc Similar to the docker build command.
% path (required) can be a local path (To a directory containing a Dockerfile) or a remote URL
build([{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker inspect command, but can only be used with an image ID.
inspect_image(ImageId) -> {error, not_implemented}.

% @doc Identical to the docker login command (But non-interactive, obviously).
login(Username, [{password, undefined}, {email, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker port command.
port(Container, PrivatePort) -> {error, not_implemented}.

% @doc Identical to the docker pull command.
pull(Repository, [{tag, undefined}, {registry, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker rmi command.
remove_image(Image) -> {error, not_implemented}.

% @doc Identical to the docker search command.
search(Term) -> {error, not_implemented}.

% @doc Identical to the docker tag command.
tag(Image, Repository, [{tag, undefined}, {force, false}] = Args) -> {error, not_implemented}.

%%%%%%%%%%%%%

default_args(build) ->
    [{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}];
default_args(commit) ->
    [{repository, undefined}, {tag, undefined}, {message, undefined}, {author, undefined},
        {conf, undefined}];
default_args(create_container) ->
    [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false},
        {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined},
        {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}];
default_args(login) ->
    [{password, undefined}, {email, undefined}];
default_args(pull) ->
    [{tag, undefined}, {registry, undefined}];
default_args(tag) ->
    [{tag, undefined}, {force, false}];

default_args(_) ->
    [].

