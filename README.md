## erldocker

`erldocker` is an Erlang application that implements [Docker Remote API v1.4](http://docs.docker.io/en/latest/api/docker_remote_api_v1.4/).

Public modules are: `docker`, `docker_container` and `docker_image`.

### Setting up

To get started, add `erldocker` app to your release applications list. If you don't use releases, check out the `Makefile`.

### Configuration

Check out `run/sys.config`. By default, `docker` listens on a unix socket serving HTTP.
Since Erlang releases before OTP 19.0 don't have anything like `gen_unix` out of the box, I'm using `erldocker_unixbridge`
that spawns `socat` to bridge `AF_UNIX` to `AF_INET`.

```erlang
    {erldocker, [
            {unixbridge_port, 32133},
            {docker_http, <<"http://localhost:32133">>}
    ]}
```

In newer versions you can set the scheme to [`http+unix`](https://github.com/benoitc/hackney/blob/21f84df8aeced6b5dc0d5de3f3be3f5d20f103f1/src/hackney_url.erl#L49):
```erlang
    {erldocker, [
            {docker_http, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock">>}
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
> docker_container:containers().
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

> docker_container:attach_stream("687264242").
{ok,<0.167.0>}

> flush().
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:18 UTC 2013\n">>}}
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:19 UTC 2013\n">>}}
Shell got {<0.167.0>,{data,<<"Wed Sep  4 08:52:20 UTC 2013\n">>}}
ok

> exit(pid(0,167,0), kill).
** exception exit: killed

> docker_container:kill("687264242").
{ok,{204,<<>>}}

> {ok, Info} = docker_container:container("687264242"),
  {_, Image} = lists:keyfind('Image', 1, Info).
{'Image',<<"b750fe79269d2ec9a3c593ef05b4332b1d1a02a62b4accb2c21d589ff2f5f2dc">>}

> docker_image:image(Image).
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
