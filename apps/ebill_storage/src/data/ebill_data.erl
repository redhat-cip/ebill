-module(ebill_data).

-behaviour(gen_server).

-include("../../include/db.hrl").

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
  start/0
]).

-export([
  count/0,
  add/4,
  add/5,
  add/6,
  find_by_metric/2
]).

% wrappers
start() ->
  couchbeam:start(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

count() ->
  gen_server:call(?MODULE, {count}).

add(ID, Resource, Metric, Value) ->
  add(ID, Resource, Metric, Value, []).
add(ID, Resource, Metric, Value, Metadatas) ->
  add(ID, Resource, Metric, Value, Metadatas, ec_date:format("Y-m-d\\TH:i:s",calendar:now_to_local_time(now()))).
add(ID, Resource, Metric, Value, Metadatas, Date) 
    when is_bitstring(ID), is_bitstring(Resource), is_list(Metadatas), is_atom(Metric), is_number(Value), is_list(Date) ->
  gen_server:call(?MODULE, {add, ID, Resource, Metric, Value, Metadatas, Date}).

find_by_metric(ID, Metric) when is_atom(Metric) ->
  gen_server:call(?MODULE, {find_by_metric, ID, Metric}).

% server

%% @hidden
init([]) ->
  {ok, init_connection()}.

%% @hidden
terminate(_Reason, _Config) -> 
  ok.

%% @hidden
handle_cast(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
handle_info(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
code_change(_OldVersion, Config, _Extra) -> 
  {ok, Config}.

handle_call({count}, _From, Config) ->
  #ebilldb{ database = Database } = Config,
  Result = case couchbeam_view:count(Database) of
    {error, Reason} -> {error, Reason};
    Count -> {ok, Count}
  end,
  {reply, Result, Config};
handle_call({add, ID, Resource, Metric, Value, Metadatas, Date}, _From, Config) ->
  #ebilldb{ database = Database } = Config,
  Doc = {[
    {<<"billing_id">>, ID},
    {<<"resource">>, Resource},
    {<<"metadatas">>, Metadatas},
    {<<"metric">>, Metric},
    {<<"value">>, Value},
    {<<"date">>, sanitize_date(Date)}
  ]},
  Result = couchbeam:save_doc(Database, Doc),
  {reply, Result, Config};
handle_call({find_by_metric, ID, Metric}, _From, Config) ->
  #ebilldb{ database = Database } = Config,
  Result = do_find_by_metric(Database, ID, Metric),
  {reply, Result, Config};
handle_call(_Message, _From, Config) ->
  {reply, error, Config}.

% private

init_connection() ->
  DBHost = ebill_config:get(db_storage_host),
  DBPort = ebill_config:get(db_storage_port),
  Connection = couchbeam:server_connection("http://" ++ DBHost ++ ":" ++ integer_to_list(DBPort), []),
  DBName = ebill_config:get(db_storage_name),
  {ok, Database} = case couchbeam:db_exists(Connection, DBName) of
    true -> couchbeam:open_db(Connection, DBName);
    false -> 
      {ok, Database1} = couchbeam:create_db(Connection, DBName),
      create_views(Database1)
  end,
  #ebilldb{
    connection = Connection,
    database = Database
  }.

sanitize_date(Date) ->
  ParsedDate = case ec_date:parse(Date) of
    {{Year, Month, Day}, {Hour, Minute, Second}} -> {{Year, Month, Day}, {Hour, Minute, Second, 0}};
    Other -> Other
  end,
  list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s.f",ParsedDate)).

create_views(Database) ->
  Views = {[
    {<<"_id">>, <<"_design/ebill">>},
    {<<"language">>,<<"javascript">>},
    {<<"views">>,
       {[{<<"find_by_resource_metric_and_dates">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.resource && doc.metric && doc.date) {\n emit([doc.billing_id, doc.resource, doc.metric, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_resource_and_dates">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.resource && doc.date) {\n emit([doc.billing_id, doc.resource, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_metrics_and_dates">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.metric && doc.date) {\n emit([doc.billing_id, doc.metric, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_dates">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.date) {\n emit([doc.billing_id, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_resource">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.resource) {\n emit([doc.billing_id, doc.resource], doc);\n}\n}">>
         }]}
       },{<<"find_by_metric">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.metric) {\n emit([doc.billing_id, doc.metric], doc);\n}\n}">>
         }]}
       },{<<"find_by_resource_and_metric">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.billing_id && doc.resource && doc.metric) {\n emit([doc.billing_id, doc.resource, doc.metric], doc);\n}\n}">>
         }]}
       }]}
    }
  ]},
  {ok, _DesignDoc1} = couchbeam:save_doc(Database, Views),
  {ok, Database}.

do_find_by_metric(Database, ID, Metric) ->
  MetricKey = [ID,Metric],
  {ok, Result} = couchbeam_view:fetch(
      Database,
      {"ebill", "find_by_metric"},
      [{key, MetricKey}]
  ),
  Result.
