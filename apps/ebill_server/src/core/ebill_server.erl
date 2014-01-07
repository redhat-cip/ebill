-module(ebill_server).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  ok = application:start(crypto),
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _Pid} = ebill_config:start(),
  ok = application:start(ebill_server).
