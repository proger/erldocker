-module(erldocker_unixbridge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API Function Exports

-export([start_link/0]).

%% gen_server Function Exports

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Socat = os:find_executable("socat"),
    Port = erlang:open_port({spawn_executable, Socat},
        [stream, exit_status, % stderr_to_stdout
            {args, ["tcp-listen:32133,reuseaddr,bind=127.0.0.1", "unix-connect:/var/run/docker.sock"]}]),

    {ok, #state{port=Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_exit, Status}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port=Port}) ->
    port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

