-module(erldocker).
-compile([export_all]).

%
% Misc
% 

% @doc Identical to the docker info command.
info() -> erldocker_api:get(info).

% @doc Identical to the docker version command.
version() -> erldocker_api:get(version).

%
% Containers
% http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#containers
%

% @doc Identical to the docker ps command.
containers() -> containers(default_args(containers)).
containers(Args) ->
    erldocker_api:get([containers, ps], Args).

% @doc Identical to the docker inspect command, but can only be used with a container ID.
container(CID) ->
    erldocker_api:get([containers, CID]).

% @doc List processes running inside the container id.
top(CID) ->
    erldocker_api:get([containers, CID, top]).

% @doc Identical to the docker diff command.
changes(CID) ->
    either:bind(erldocker_api:get([containers, CID, changes]), fun changes_proc/1).
diff(CID) -> changes(CID).

changes_proc(Changes) ->
    changes_proc(Changes, []).
changes_proc([{[{<<"Path">>,Path},{<<"Kind">>,Kind}]}|Rest], Acc) ->
    changes_proc(Rest, [{Path, Kind}|Acc]);
changes_proc([], Acc) ->
    lists:reverse(Acc).

% @doc Identical to the docker export command.
export(CID) -> 
    erldocker_api:get([containers, CID, export]).

% @doc Start the container.
start(CID, _Config) ->
    erldocker_api:post([containers, CID, start]).

% @doc Allows to bind a directory in the host to the container.
% Similar to the docker run command with the -b="/host:/mnt".
% Requires the container to be created with the volumes argument: c.create_container(..., volumes=[{'/mnt': {}}] = Args)
% start_with_binds(Container, Binds) ->
%     {error, not_implemented}.

% @doc Stop the container.
stop(CID) -> stop(CID, default_args(stop)).
stop(CID, Args) ->
    erldocker_api:post([containers, CID, stop], Args).

% @doc Restart the container.
restart(CID) -> restart(CID, default_args(restart)).
restart(CID, Args) ->
    erldocker_api:post([containers, CID, restart], Args).

% @doc Identical to the docker kill command.
kill(CID, _Config) ->
    erldocker_api:post([containers, CID, kill]).

% @doc Attach to container.
attach(CID) -> attach(CID, default_args(attach)).
attach(CID, Args) ->
    erldocker_api:post([containers, CID, attach], Args).

% @doc Attach to container. Starts sending messages to calling process. Returns {ok, Pid}.
attach_stream(CID) ->
    erldocker_api:post_stream([containers, CID, attach], [stream, stdout, stderr]).

% @doc Identical to the docker rm command.
delete(CID) -> delete(CID, default_args(delete)).
delete(CID, Args) ->
    erldocker_api:delete([containers, CID], Args).

% @doc Identical to the docker wait command.
wait(CID) ->
    erldocker_api:post([containers, CID, wait]).

% @doc Copy files or folders of container.
copy(_CID, _Args) ->
    {error, not_implemented}.

%%%%% docker-py COPY-PASTE BELOW

% @doc Creates a container that can then be started.
% Parameters are similar to those for the docker run command except it doesn't support the attach options (-a)
create_container(Image, Command, [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false}, {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined}, {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}] = Args) -> {error, not_implemented}.

% @doc Similar to the docker build command.
% path (required) can be a local path (To a directory containing a Dockerfile) or a remote URL
build([{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker commit command.
commit(Container, [{repository, undefined}, {tag, undefined}, {message, undefined}, {author, undefined}, {conf, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker history command.
history(Image) -> {error, not_implemented}.

% @doc Identical to the docker images command.
images([{name, undefined}, {quiet, false}, {all, false}, {viz, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker import command.
% If src is a string or unicode string, it will be treated as a URL to fetch the image from.
% To import an image from the local machine, src needs to be a file-like object or bytes collection.
import_image(Src, [{repository, undefined}, {tag, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker insert command.
insert(Url, Path) -> {error, not_implemented}.

% @doc Identical to the docker inspect command, but can only be used with an image ID.
inspect_image(ImageId) -> {error, not_implemented}.

% @doc Identical to the docker login command (But non-interactive, obviously).
login(Username, [{password, undefined}, {email, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker logs command.
logs(Container) -> {error, not_implemented}.

% @doc Identical to the docker port command.
port(Container, PrivatePort) -> {error, not_implemented}.

% @doc Identical to the docker pull command.
pull(Repository, [{tag, undefined}, {registry, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker push command.
push(Repository) -> {error, not_implemented}.

% @doc Identical to the docker rmi command.
remove_image(Image) -> {error, not_implemented}.

% @doc Identical to the docker search command.
search(Term) -> {error, not_implemented}.

% @doc Identical to the docker tag command.
tag(Image, Repository, [{tag, undefined}, {force, false}] = Args) -> {error, not_implemented}.

%%%%%%%%%%%%%

default_args(containers) ->
    [{quiet, false}, {all, false}, trunc, {latest, false}, {since, undefined},
        {before, undefined}, {limit, -1}];
default_args(delete) ->
    [{v, false}];
default_args(restart) ->
    [{t, 10}];
default_args(stop) ->
    [{t, 10}];
default_args(attach) ->
    [{logs, false}, {stream, false}, {stdin, false}, {stdout, false}, {stderr, false}];

default_args(build) ->
    [{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}];
default_args(commit) ->
    [{repository, undefined}, {tag, undefined}, {message, undefined}, {author, undefined},
        {conf, undefined}];
default_args(create_container) ->
    [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false},
        {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined},
        {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}];
default_args(images) ->
    [{name, undefined}, {quiet, false}, {all, false}, {viz, false}];
default_args(import_image) ->
    [{repository, undefined}, {tag, undefined}];
default_args(login) ->
    [{password, undefined}, {email, undefined}];
default_args(pull) ->
    [{tag, undefined}, {registry, undefined}];
default_args(tag) ->
    [{tag, undefined}, {force, false}];

default_args(_) ->
    [].

