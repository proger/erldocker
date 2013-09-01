-module(erldocker_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _Pid} = hackney:start_pool(erldocker_pool, [{timeout, 150000}, {pool_size, 1}]),

    erldocker_sup:start_link().

stop(_State) ->
    hackney:stop_pool(erldocker_pool),

    ok.
