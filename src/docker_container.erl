-module(docker_container).
-compile([export_all]).
-include("erldocker.hrl").

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

default_args(_) ->
    [].
