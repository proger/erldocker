-module(erldocker_unixbridge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API Function Exports

-export([start_link/1]).

%% gen_server Function Exports

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(BindPort) when is_integer(BindPort)->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BindPort], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([BindPort]) ->
    Socat = case os:find_executable("socat") of
        false -> throw(socat_not_found);
        S -> S
    end,
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
        [stream, exit_status, % stderr_to_stdout
            {args, [Socat,
                    "tcp-listen:" ++ integer_to_list(BindPort) ++ ",reuseaddr,bind=127.0.0.1,fork",
                    "unix-connect:/var/run/docker.sock"]}]),

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
    (catch port_close(Port)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

