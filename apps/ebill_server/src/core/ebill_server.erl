-module(ebill_server).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(cowboy),
  ok = application:start(ebill_config),
  ok = application:start(ebill_server).
