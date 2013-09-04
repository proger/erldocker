## erldocker

`erldocker` is an Erlang application that implements [Docker Remote API v1.4](http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/).

Public modules are: `docker`, `docker_container` and `docker_image`.

### Setting up

To get started, add `erldocker` app to your release applications list. If you don't use releases, check out the `Makefile`.

### Configuration

Check out `run/sys.config`. By default, `docker` listens on the unix socket serving HTTP.
Since Erlang doesn't have something like `gen_unix` by default, I'm using `erldocker_unixbridge`
that spawns `socat` to bridge `AF_UNIX` to `AF_INET`.

```erlang
    {erldocker, [
            {unixbridge_port, 32133},
            {docker_http, <<"http://localhost:32133">>}
    ]}
```

If your docker is configured to listen on a TCP port, you may use the following configuration:

```erlang
    {erldocker, [
            {docker_http, <<"http://localhost:4243">>}
    ]}
```

### Erlang APIs

The APIs are pretty straightforward in general.
Please note that the library is under heavy development and some of the APIs (namely the ones that use POST bodies)
are not very nice as of right now. This will change without further notice.

Check out the examples:

```erlang
(erldocker@precise64)3> docker_container:containers().
08:50:40.695 [info] api call: get http://localhost:32133/containers/ps?quiet=false&all=false&trunc=true&latest=false&limit=-1
{ok,[[{'Id',<<"6274938616c28e82b49b3ab120f9e9b9d96592d3b5bb95b53e91f740aca76243">>},
      {'Image',<<"base:latest">>},
      {'Command',<<"/bin/bash -c while true; do sleep 1; date; done">>},
      {'Created',1378278965},
      {'Status',<<"Up About an hour">>},
      {'Ports',<<>>},
      {'SizeRw',0},
      {'SizeRootFs',0}],
     [{'Id',<<"a574cdc01fa64e9b7e359d8d76f734505a4d9d121e39af877fab8c14a5d93baf">>},
      {'Image',<<"base:latest">>},
      {'Command',<<"/bin/bash -c while true; do sleep 1; date; done">>},
      {'Created',1378278960},
      {'Status',<<"Up About an hour">>},
      {'Ports',<<>>},
      {'SizeRw',0},
      {'SizeRootFs',0}]]}

(erldocker@precise64)4> docker_container:attach_stream("687264242").
08:52:17.626 [info] api call: {post,stream} http://localhost:32133/containers/687264242/attach?stream=true&stdout=true&stderr=true
{ok,<0.167.0>}
(erldocker@precise64)5> flush().
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:18 UTC 2013\n">>}}
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:19 UTC 2013\n">>}}
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:20 UTC 2013\n">>}}
ok
(erldocker@precise64)6> exit(pid(0,167,0), kill).
** exception exit: killed

(erldocker@precise64)9> docker_container:kill("687264242").
08:53:07.919 [info] api call: post http://localhost:32133/containers/687264242/kill
{ok,{204,<<>>}}

(erldocker@precise64)11> {ok, Info} = docker_container:container("687264242"), {_, Image} = lists:keyfind('Image', 1, Info).
08:54:27.886 [info] api call: get http://localhost:32133/containers/687264242/json
{'Image',<<"b750fe79269d2ec9a3c593ef05b4332b1d1a02a62b4accb2c21d589ff2f5f2dc">>}
(erldocker@precise64)12> docker_image:image(Image).
08:54:43.775 [info] api call: get http://localhost:32133/images/b750fe79269d2ec9a3c593ef05b4332b1d1a02a62b4accb2c21d589ff2f5f2dc/json
{ok,[{id,<<"b750fe79269d2ec9a3c593ef05b4332b1d1a02a62b4accb2c21d589ff2f5f2dc">>},
     {parent,<<"27cf784147099545">>},
     {created,<<"2013-03-23T22:24:18.818426-07:00">>},
     {container,<<"3d67245a8d72ecf13f33dffac9f79dcdf70f75acb84d308770391510e0c23ad0">>},
     {container_config,[{'Hostname',<<>>},
                        {'User',<<>>},
                        {'Memory',0},
                        {'MemorySwap',0},
                        {'CpuShares',0},
                        {'AttachStdin',false},
                        {'AttachStdout',false},
                        {'AttachStderr',false},
                        {'PortSpecs',null},
                        {'Tty',true},
                        {'OpenStdin',true},
                        {'StdinOnce',false},
                        {'Env',null},
                        {'Cmd',[<<"/bin/bash">>]},
                        {'Dns',null},
                        {'Image',<<"base">>},
                        {'Volumes',null},
                        {'VolumesFrom',<<>>},
                        {'WorkingDir',...},
                        {...}|...]},
     {'Size',24653}]}
```
