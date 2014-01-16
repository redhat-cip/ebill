-module(ebill_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  lager:info("_StartArgs = ~p / Env = ~p", [_StartArgs, application:get_all_env()]),
  ebill_pool_sup:start_link().

stop(_State) ->
  ok.
