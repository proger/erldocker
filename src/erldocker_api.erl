-module(erldocker_api).
-export([get/1, get/2, post/1, post/2, delete/1, delete/2, post_stream/1, post_stream/2]).

-define(ADDR, application:get_env(erldocker, docker_http, <<"http://localhost:4243">>)).
-define(OPTIONS, [{pool, erldocker_pool}]).

get(URL) -> call(get, URL).
get(URL, Args) -> call(get, URL, Args).
post(URL) -> call(post, URL).
post(URL, Args) -> call(post, URL, Args).
delete(URL) -> call(delete, URL).
delete(URL, Args) -> call(delete, URL, Args).
post_stream(URL) -> call(post_stream, URL).
post_stream(URL, Args) -> call(post_stream, URL, Args).

call(post_stream, URL) when is_binary(URL) ->
    error_logger:info_msg("api call: ~p ~s", [post_stream, binary_to_list(URL)]),
    case hackney:request(post, URL, [], <<>>, ?OPTIONS) of
        {ok, StatusCode, _RespHeaders, Client} ->
            case StatusCode of
                X when X == 200 orelse X == 201 orelse X == 204 ->
                    Rcv = self(),
                    Pid = spawn_link(fun() -> read_body(Rcv, Client) end),
                    {ok, Pid};
                _ ->
                    {error, StatusCode}
            end;
        {error, _} = E ->
            E
    end;

call(Method, URL) when is_binary(URL) ->
    error_logger:info_msg("api call: ~p ~s", [Method, binary_to_list(URL)]),
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, URL, ReqHeaders, <<>>, ?OPTIONS) of
        {ok, StatusCode, _RespHeaders, Client} ->
            {ok, Body, _Client1} = hackney:body(Client),
            case StatusCode of
                X when X == 200 orelse X == 201 orelse X == 204 ->
                    {ok, jiffy:decode(Body)};
                _ ->
                    {error, {StatusCode, Body}}
            end;
        {error, _} = E ->
            E
    end;
call(Method, URL) ->
    call(Method, to_url(URL)).
call(Method, URL, Args) ->
    call(Method, to_url(URL, Args)).

read_body(Receiver, Client) ->
    case hackney:stream_body(Client) of
        {ok, Data, Client2} ->
            Receiver ! {self(), {data, Data}},
            read_body(Receiver, Client2);
        {done, Client2} ->
            Receiver ! {self(), {data, eof}},
            {ok, Client2};
        {error, _Reason} = E->
            Receiver ! {self(), E},
            E
    end.

argsencode([], Acc) ->
    hackney_util:join(lists:reverse(Acc), <<"&">>);
argsencode ([{_K,undefined}|R], Acc) ->
    argsencode(R, Acc);
argsencode ([{K,V}|R], Acc) ->
    K1 = hackney_url:urlencode(to_binary(K)),
    V1 = hackney_url:urlencode(to_binary(V)),
    Line = << K1/binary, "=", V1/binary >>,
    argsencode(R, [Line | Acc]);
argsencode([K|R], Acc) ->
    argsencode([{K, <<"true">>}|R], Acc).

to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_binary(X) -> X.

convert_url_parts(L) when is_list(L) -> convert_url_parts(L, []);
convert_url_parts(X) -> convert_url_parts([X], []).

convert_url_parts([X|Xs], Acc) ->
    convert_url_parts(Xs, [<<"/", (to_binary(X))/binary>>|Acc]);
convert_url_parts([], Acc) ->
    lists:reverse(Acc).

to_url(X) ->
    iolist_to_binary([?ADDR|convert_url_parts(X)]).

to_url(X, []) ->
    to_url(X);
to_url(X, Args) ->
    iolist_to_binary([?ADDR, convert_url_parts(X), <<"?">>, argsencode(Args, [])]).

