-module(ebill_storage).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  ok = application:start(crypto),
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _Pid} = ebill_config:start(),
  {ok, _Pid2} = ebill_data:start(),
  ok = application:start(ebill_storage).
