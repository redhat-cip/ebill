-module(ebill_storage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Routes      = routes(),
  Dispatch    = cowboy_router:compile(Routes),
  Port        = ebill_config:get(tcp_storage_port),
  TransOpts   = [{port, Port}],
  ProtoOpts   = [ {env, [{dispatch, Dispatch}]} ],
  {ok, _}     = cowboy:start_http(http, ebill_config:get(max_storage_conn), TransOpts, ProtoOpts),
  lager:info("Billing storage started on port ~p (~p)", [Port, code:priv_dir(ebill_storage)]),
  ebill_storage_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
  [
    {'_', [
      {"/", ebill_storage_handler, []},
      {"/metrics", ebill_metrics_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, ebill_storage, "static", [
        {mimetypes, cow_mimetypes, all}
      ]}}
    ]}
  ].


