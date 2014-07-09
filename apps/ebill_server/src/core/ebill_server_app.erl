-module(ebill_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Routes      = routes(),
  Dispatch    = cowboy_router:compile(Routes),
  Port        = ebill_config:get(tcp_server_port),
  TransOpts   = [{port, Port}],
  ProtoOpts   = [ {env, [{dispatch, Dispatch}]} ],
  {ok, _}     = cowboy:start_http(http, ebill_config:get(max_server_conn), TransOpts, ProtoOpts),
  lager:info("Billing server started on port ~p (~p)", [Port, code:priv_dir(ebill_server)]),
  ebill_server_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
  [
    {'_', [
      {"/", ebill_server_handler, []},
      {"/db", ebill_redirect_db_handler, []},
      {"/cost", ebill_cost_handler, []},
      {"/charging", ebill_bill_handler, []},
      {"/template/:id", ebill_template_handler, []},
      {"/template", ebill_template_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, ebill_server, "static", [
        {mimetypes, cow_mimetypes, all}
      ]}},
      {"/browser/[...]", cowboy_static, {priv_dir, ebill_server, "browser", [
        {mimetypes, cow_mimetypes, all}
      ]}}
    ]}
  ].
