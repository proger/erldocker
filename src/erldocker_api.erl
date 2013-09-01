-module(erldocker_api).
-compile([export_all]).
-define(OPTIONS, [{pool, erldocker_pool}]).

get(URL) ->
    URL2 = "http://localhost:32133" ++ URL,
    {ok, StatusCode, RespHeaders, Client} = hackney:get(URL2, [], <<>>, ?OPTIONS),
    {ok, Body, Client1} = hackney:body(Client),
    jiffy:decode(Body).

