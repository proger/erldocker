-module(docker_image).
-include("erldocker.hrl").

-export([images/0, images/1]).
-export([image/1]).
-export([create/1]).
-export([build/2]).
-export([insert/3]).
-export([history/1]).
-export([tag/2, tag/3]).
-export([delete/1]).
-export([search/1]).

-export([push/2]). % not implemented

-export([default_args/1]).

%
% Images
% @doc http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#images
%

% @doc Returns list of images.
images() -> images(default_args(images)).
images(Args) ->
    ?PROPLISTS(erldocker_api:get([images, json], Args)).

% @doc Return low-level information on the image.
image(I) ->
    ?PROPLIST(erldocker_api:get([images, I, json])).

% @doc Create an image, either by pull it from the registry or by importing it.
create(Args) ->
    erldocker_api:post([images, create], Args).

% @doc Build an image from Dockerfile in the tarball.
% http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/#build-an-image-from-dockerfile-via-stdin
build(TarBin, Args) ->
    erldocker_api:post(build, Args, TarBin).

% @doc Insert a file from Url in the image name at Path.
insert(I, Url, Path) ->
    erldocker_api:post([images, I, insert], [{path, Path}, {url, Url}]).

% @doc Return the history of the image.
history(I) ->
    ?PROPLISTS(erldocker_api:get([images, I, history])).

% @doc Push the image name on the registry.
push(_Repo, _AuthConfig) ->
    {error, not_implemented}.

% @doc Tag the image into a repository.
tag(I, Repo) -> tag(I, Repo, default_args(tag)).
tag(I, Repo, Args) ->
    erldocker_api:post([images, I, tag], [{repo, Repo}|Args]).

% @doc Remove the image from the filesystem.
delete(I) ->
    ?PROPLISTS(erldocker_api:delete([images, I])).

% @doc Search for an image in the docker index.
search(Term) ->
    ?PROPLISTS(erldocker_api:get([images, search], [{term, Term}])).


default_args(images) ->
    [{all, false}];
default_args(create) ->
    [{repo, undefined}, {tag, undefined}, {fromImage, undefined}, {fromSrc, undefined}, {registry, undefined}];
default_args(tag) ->
    [{tag, undefined}, {force, false}];
default_args(build) ->
    [{t, undefined}, {q, false}, {nocache, false}];

default_args(_) ->
    [].
