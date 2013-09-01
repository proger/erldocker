-module(erldocker).
-compile([export_all]).

% @doc Similar to the docker build command.
% Either path or fileobj needs to be set.
% path can be a local path (To a directory containing a Dockerfile) or a remote URL.
% fileobj must be a readable file-like object to a Dockerfile.
build([{path, undefined}, {tag, undefined}, {quiet, false}, {nocache, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker commit command.
commit(Container, [{repository, undefined}, {tag, undefined}, {message, undefined}, {author, undefined}, {conf, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker ps command.
containers([{quiet, false}, {all, false}, {trunc, true}, {latest, false}, {since, undefined}, {before, undefined}, {limit, -1}] = Args) -> {error, not_implemented}.

% @doc Creates a container that can then be started.
% Parameters are similar to those for the docker run command except it doesn't support the attach options (-a)
create_container(Image, Command, [{hostname, undefined}, {user, undefined}, {detach, false}, {stdin_open, false}, {tty, false}, {mem_limit, 0}, {ports, undefined}, {environment, undefined}, {dns, undefined}, {volumes, undefined}, {volumes_from, undefined}, {privileged, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker diff command.
diff(Container) -> {error, not_implemented}.

% @doc Identical to the docker export command.
export(Container) -> {error, not_implemented}.

% @doc Identical to the docker history command.
history(Image) -> {error, not_implemented}.

% @doc Identical to the docker images command.
images([{name, undefined}, {quiet, false}, {all, false}, {viz, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker import command.
% If src is a string or unicode string, it will be treated as a URL to fetch the image from.
% To import an image from the local machine, src needs to be a file-like object or bytes collection.
import_image(Src, [{repository, undefined}, {tag, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker info command.
info() -> erldocker_api:get("/info").

% @doc Identical to the docker insert command.
insert(Url, Path) -> {error, not_implemented}.

% @doc Identical to the docker inspect command, but can only be used with a container ID.
inspect_container(Container_id) -> {error, not_implemented}.

% @doc Identical to the docker inspect command, but can only be used with an image ID.
inspect_image(Container_id) -> {error, not_implemented}.

% @doc Identical to the docker kill command.
kill(Containers) when is_list(Containers) -> {error, not_implemented}.

% @doc Identical to the docker login command (But non-interactive, obviously).
login(Username, [{password, undefined}, {email, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker logs command.
logs(Container) -> {error, not_implemented}.

% @doc Identical to the docker port command.
port(Container, Private_port) -> {error, not_implemented}.

% @doc Identical to the docker pull command.
pull(Repository, [{tag, undefined}, {registry, undefined}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker push command.
push(Repository) -> {error, not_implemented}.

% @doc Identical to the docker rm command.
remove_container(Containers, [{v, false}] = Args) when is_list(Containers) -> {error, not_implemented}.

% @doc Identical to the docker rmi command.
remove_image(Images) when is_list(Images) -> {error, not_implemented}.

% @doc Identical to the docker restart command.
restart(Containers, [{t, 10}] = Args) when is_list(Containers) -> {error, not_implemented}.

% @doc Identical to the docker search command.
search(Term) -> {error, not_implemented}.

% @doc Identical to the docker start command, but doesn't support attach options.
% Use docker logs to recover stdout/stderr
start(Container) -> {error, not_implemented}.

% @doc Allows to bind a directory in the host to the container.
% )imilar to the docker run command with the -b="/host:/mnt".
% Requires the container to be created with the volumes argument: c.create_container(..., volumes=[{'/mnt': {}}] = Args)
start(Container, Binds) -> {error, not_implemented}.

% @doc Identical to the docker stop command.
stop(Containers, [{t, 10}] = Args) when is_list(Containers) -> {error, not_implemented}.

% @doc Identical to the docker tag command.
tag(Image, Repository, [{tag, undefined}, {force, false}] = Args) -> {error, not_implemented}.

% @doc Identical to the docker version command.
version() -> erldocker_api:get("/version").

% @doc Identical to the docker wait command.
wait(Containers) when is_list(Containers) -> {error, not_implemented}.
