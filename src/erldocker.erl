-module(erldocker).
-compile([export_all]).

% @doc Similar to the docker build command.
% Either path or fileobj needs to be set.
% path can be a local path (To a directory containing a Dockerfile) or a remote URL.
% fileobj must be a readable file-like object to a Dockerfile.
build([{path, none}, {tag, none}, {quiet, false}, {fileobj, none}, {nocache, false}] = Args) -> ok.

% @doc Identical to the docker commit command.
commit(Container, [{repository, none}, {tag, none}, {message, none}, {author, none}, {conf, none}] = Args) -> ok.

% @doc Identical to the docker ps command.
containers([{quiet, false}, {all, false}, {trunc, true}, {latest, false}, {since, none}, {before, none}, {limit, -1}] = Args) -> ok.

% @doc Creates a container that can then be started.
% Parameters are similar to those for the docker run command except it doesn't support the attach options (-a)
create_container(Image, command, [{hostname, none}, {user, none}, {detach, false}, {stdin_open, false}, {tty, false}, {mem_limit, 0}, {ports, none}, {environment, none}, {dns, none}, {volumes, none}, {volumes_from, none}, {privileged, false}] = Args) -> ok.

% @doc Identical to the docker diff command.
diff(Container) -> ok.

% @doc Identical to the docker export command.
export(Container) -> ok.

% @doc Identical to the docker history command.
history(Image) -> ok.

% @doc Identical to the docker images command.
images([{name, none}, {quiet, false}, {all, false}, {viz, false}] = Args) -> ok.

% @doc Identical to the docker import command.
% If src is a string or unicode string, it will be treated as a URL to fetch the image from.
% To import an image from the local machine, src needs to be a file-like object or bytes collection.
import_image(Src, [{repository, none}, {tag, none}] = Args) -> ok.

% @doc Identical to the docker info command.
info() -> ok.

% @doc Identical to the docker insert command.
insert(Url, path) -> ok.

% @doc Identical to the docker inspect command, but can only be used with a container ID.
inspect_container(Container_id) -> ok.

% @doc Identical to the docker inspect command, but can only be used with an image ID.
inspect_image(Container_id) -> ok.

% @doc Identical to the docker kill command.
kill(Containers) when is_list(Containers) -> ok.

% @doc Identical to the docker login command (But non-interactive, obviously).
login(Username, [{password, none}, {email, none}] = Args) -> ok.

% @doc Identical to the docker logs command.
logs(Container) -> ok.

% @doc Identical to the docker port command.
port(Container, private_port) -> ok.

% @doc Identical to the docker pull command.
pull(Repository, [{tag, none}, {registry, none}] = Args) -> ok.

% @doc Identical to the docker push command.
push(Repository) -> ok.

% @doc Identical to the docker rm command.
remove_container(Containers, [{v, false}] = Args) when is_list(Containers) -> ok.

% @doc Identical to the docker rmi command.
remove_image(Images) when is_list(Images) -> ok.

% @doc Identical to the docker restart command.
restart(Containers, [{t, 10}] = Args) when is_list(Containers) -> ok.

% @doc Identical to the docker search command.
search(Term) -> ok.

% @doc Identical to the docker start command, but doesn't support attach options.
% Use docker logs to recover stdout/stderr
start(Container) -> ok.

% @doc Allows to bind a directory in the host to the container.
% )imilar to the docker run command with the -b="/host:/mnt".
% Requires the container to be created with the volumes argument: c.create_container(..., volumes=[{'/mnt': {}}] = Args)
start(Container, Binds) -> ok.

% @doc Identical to the docker stop command.
stop(Containers, [{t, 10}] = Args) when is_list(Containers) -> ok.

% @doc Identical to the docker tag command.
tag(Image, repository, [{tag, none}, {force, false}] = Args) -> ok.

% @doc Identical to the docker version command.
version() -> ok.

% @doc Identical to the docker wait command.
wait(Containers) when is_list(Containers) -> ok.
