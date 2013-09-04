-module(docker_image).
-compile([export_all]).
-include("erldocker.hrl").

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

% @doc TODO
image_import(_) ->
    {error, not_implemented}.

% @doc Identical to the docker insert command.
image_insert(I, Url, Path) ->
    {error, not_implemented}.

% @doc Identical to the docker history command.
history(I) ->
    ?PROPLISTS(erldocker_api:get([images, I, history])).

% @doc Identical to the docker push command.
push(Repository) ->
    {error, not_implemented}.

% @doc Identical to the docker commit command.
commit(CID, Args) ->
    {error, not_implemented}.


default_args(images) ->
    [{all, false}];
default_args(image_import) ->
    [{repository, undefined}, {tag, undefined}, {fromImage, undefined}, {fromSrc, undefined}, {registry, undefined}];

default_args(_) ->
    [].
