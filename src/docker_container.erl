-module(docker_container).
-include("erldocker.hrl").

-export([containers/0, containers/1]).
-export([container/1]).
-export([create/1]).
-export([top/1]).
-export([changes/1, diff/1]).
-export([export/1]).
-export([start/2]).
-export([stop/1, stop/2]).
-export([restart/1, restart/2]).
-export([kill/1]).
-export([attach_logs/1, attach_stream/1, attach_stream_with_logs/1]).
-export([delete/1, delete/2]).
-export([wait/1]).
-export([commit/1, commit/2]).

-export([copy/2]). % not implemented

-export([default_args/1]).

%
% Containers
% http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#containers
%

% @doc Identical to the docker ps command.
containers() -> containers(default_args(containers)).
containers(Args) ->
    ?PROPLISTS(erldocker_api:get([containers, ps], Args)).

% @doc Identical to the docker inspect command, but can only be used with a container ID.
container(CID) ->
    ?PROPLIST(erldocker_api:get([containers, CID, json])).

% @doc Creates a container that can then be started.
% http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#create-a-container
create(ConfigBin) ->
    erldocker_api:post([containers, create], ConfigBin).

% @doc List processes running inside the container id.
top(CID) ->
    erldocker_api:get([containers, CID, top]).

% @doc Identical to the docker diff command.
changes(CID) ->
    ?PROPLISTS(erldocker_api:get([containers, CID, changes])).
diff(CID) -> changes(CID).

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
kill(CID) ->
    erldocker_api:post([containers, CID, kill]).

% @doc Attach to container and grab logs.
attach_logs(CID) ->
    either:bind(erldocker_api:post_stream([containers, CID, attach], [logs, stdout, stderr]), fun logs_proc/1).

logs_proc(Pid) -> {ok, logs_proc(Pid, [])}.
logs_proc(Pid, Acc) ->
    receive
        {Pid, {error, _}} -> iolist_to_binary(lists:reverse(Acc));
        {Pid, {data, eof}} -> iolist_to_binary(lists:reverse(Acc));
        {Pid, {data, D}} -> logs_proc(Pid, [D|Acc])
    end.

% @doc Attach to container. Starts sending messages to calling process with output. Returns {ok, Pid}.
attach_stream(CID) ->
    erldocker_api:post_stream([containers, CID, attach], [stream, stdout, stderr]).

% @doc Attach to container. Starts sending messages to calling process with output since the container start. Returns {ok, Pid}.
attach_stream_with_logs(CID) ->
    erldocker_api:post_stream([containers, CID, attach], [stream, logs, stdout, stderr]).

% @doc Identical to the docker rm command.
delete(CID) -> delete(CID, default_args(delete)).
delete(CID, Args) ->
    erldocker_api:delete([containers, CID], Args).

% @doc Identical to the docker wait command.
wait(CID) ->
    either:bind(erldocker_api:post([containers, CID, wait]), fun wait_proc/1).

wait_proc({[{<<"StatusCode">>, Status}]}) -> {ok, Status}.

% @doc Copy files or folders of container.
copy(_CID, _Args) ->
    {error, not_implemented}.

% @doc Create a new image from a container’s changes.
commit(CID) -> commit(CID, default_args(commit)).
commit(CID, Args) ->
    ?PROPLIST(erldocker:post([commit], [{container, CID}|Args])).

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
    [{logs, true}, {stream, false}, {stdin, false}, {stdout, true}, {stderr, true}];
default_args(commit) ->
    % http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#id34
    [{repo, undefined}, {tag, undefined}, {m, undefined}, {author, undefined}, {run, undefined}];
default_args(create) ->
    [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false},
        {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined},
        {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}];

default_args(_) ->
    [].
