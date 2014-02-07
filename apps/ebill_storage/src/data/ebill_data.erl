-module(ebill_data).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../../include/db.hrl").

-export([
  start_link/0,
  count/0,
  add/4,
  add/5,
  add/6,
  del/1,
  metric_list/0,
  metric_list/1,
  find/1,
  find_by/2,
  find_by_id/1,
  find_by_metric/2,
  find_by_resource/2,
  find_by_resource_and_metric/3,
  find_by_date/2,
  find_by_metric_and_date/3,
  find_by_resource_and_date/3,
  find_by_resource_metric_and_date/4
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

% wrappers
start_link() ->
  couchbeam:start(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

count() ->
  gen_server:call(?SERVER, {count}).

add(ID, Resource, Metric, Value) ->
  add(ID, Resource, Metric, Value, []).
add(ID, Resource, Metric, Value, Metadatas) ->
  add(ID, Resource, Metric, Value, Metadatas, ec_date:format("Y-m-d\\TH:i:s",calendar:now_to_local_time(now()))).
add(ID, Resource, Metric, Value, Metadatas, Date) 
    when is_bitstring(ID), is_bitstring(Resource), is_list(Metadatas), is_atom(Metric), is_bitstring(Date) ->
  gen_server:call(?SERVER, {add, ID, Resource, Metric, Value, Metadatas, Date}).

del(Doc) ->
  gen_server:call(?SERVER, {del, Doc}).

find_by(ID, Datas) ->
  V0 = 0,
  {V1, Resource} = case lists:keyfind(resource, 1, Datas) of
    {resource, R} -> {V0+1, R};
    false -> {V0, undefined}
  end,
  {V2, Metric} = case lists:keyfind(metric, 1, Datas) of
    {metric, M} -> {V1+2, M};
    false -> {V1, undefined}
  end,
  {V3, DateInterval} = case lists:keyfind(dates, 1, Datas) of
    {dates, D} -> {V2+4, D};
    false -> {V2, undefined}
  end,
  case V3 of
    0 -> find_by_id(ID);
    1 -> find_by_resource(ID, Resource);
    2 -> find_by_metric(ID, Metric);
    3 -> find_by_resource_and_metric(ID, Resource, Metric);
    4 -> find_by_date(ID, DateInterval);
    5 -> find_by_resource_and_date(ID, Resource, DateInterval);
    6 -> find_by_metric_and_date(ID, Metric, DateInterval);
    7 -> find_by_resource_metric_and_date(ID, Resource, Metric, DateInterval);
    _ -> {error, invalid}
  end.

find_by_id(ID) ->
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_id", [{options, [{key, ID}]}]}).

find_by_metric(ID, Metric) when is_atom(Metric) ->
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_metric", [{options, [{key, [ID, Metric]}]}]}).

find_by_resource(ID, Resource) when is_bitstring(Resource) ->
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_resource", [{options, [{key, [ID, Resource]}]}]}).

find_by_resource_and_metric(ID, Resource, Metric) when is_bitstring(Resource), is_atom(Metric) ->
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_resource_and_metric", [{options, [{key, [ID, Resource, Metric]}]}]}).

% DateInterval = [{start_date, StartDate}, {end_date, EndDate}]
find_by_date(ID, DateInterval) ->
  {Start, End} = do_date_interval([ID], DateInterval),
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_date", [{options, [{startkey, Start}, {endkey, End}]}]}).

find_by_metric_and_date(ID, Metric, DateInterval) ->
  {Start, End} = do_date_interval([ID, Metric], DateInterval),
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_metric_and_date", [{options, [{startkey, Start}, {endkey, End}]}]}).

find_by_resource_and_date(ID, Resource, DateInterval) ->
  {Start, End} = do_date_interval([ID, Resource], DateInterval),
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_resource_and_date", [{options, [{startkey, Start}, {endkey, End}]}]}).

find_by_resource_metric_and_date(ID, Resource, Metric, DateInterval) ->
  {Start, End} = do_date_interval([ID, Resource, Metric], DateInterval),
  gen_server:call(?SERVER, {parse_map_reduce, "find_by_resource_metric_and_date", [{options, [{startkey, Start}, {endkey, End}]}]}).

metric_list() ->
  gen_server:call(?SERVER, {unparse_map_result, "metric_list", [{options, [reduce, group]}, {extract, fun({E}, Acc) -> 
              {<<"key">>, K} = lists:keyfind(<<"key">>, 1, E),
              Acc ++ [binary_to_atom(K, utf8)]
          end}]}).

metric_list(ID) ->
  gen_server:call(?SERVER, {unparse_map_result, "metric_list_for_id", [{options, [reduce, group]}, {extract, fun({E}, Acc) ->
              {<<"key">>, K} = lists:keyfind(<<"key">>, 1, E),
              case lists:member(ID, K) of
                true ->
                  [M|_] = lists:delete(ID, K),
                  Acc ++ [binary_to_atom(M, utf8)];
                _ -> Acc
              end
          end}]}).

% server

%% @hidden
init([]) ->
  {ok, init_connection()}.

%% @hidden
terminate(_Reason, _Config) -> 
  couchbeam:stop(),
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
    Count -> {ok, Count - 1} % We must remove _design view
  end,
  {reply, Result, Config};
handle_call({add, ID, Resource, Metric, Value, Metadatas, Date}, _From, Config) ->
  #ebilldb{ database = Database } = Config,
  Doc = {[
    {<<"project_id">>, ID},
    {<<"resource_id">>, Resource},
    {<<"metadatas">>, Metadatas},
    {<<"metric">>, Metric},
    {<<"value">>, Value},
    {<<"date">>, sanitize_date(Date)}
  ]},
  Result = couchbeam:save_doc(Database, Doc),
  {reply, Result, Config};
handle_call({del, Doc}, _From, Config) ->
  #ebilldb{ database = Database } = Config,
  Result = couchbeam:delete_doc(Database, Doc),
  {reply, Result, Config};
handle_call({unparse_map_result, ViewName, Options}, _From, Config) ->
  Result = do_map_reduce(ViewName, Options, Config),
  {reply, Result, Config};
handle_call({parse_map_reduce, ViewName, Options}, _From, Config) ->
  Result = do_parse_map_reduce(do_map_reduce(ViewName, Options, Config)),
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
  ParsedDate = case ec_date:parse(bitstring_to_list(Date)) of
    {{Year, Month, Day}, {Hour, Minute, Second}} -> {{Year, Month, Day}, {Hour, Minute, Second, 0}};
    Other -> Other
  end,
  list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s.f",ParsedDate)).

create_views(Database) ->
  Views = {[
    {<<"_id">>, <<"_design/ebill">>},
    {<<"language">>,<<"javascript">>},
    {<<"views">>,
       {[{<<"find_by_resource_metric_and_date">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.resource_id && doc.metric && doc.date) {\n emit([doc.project_id, doc.resource_id, doc.metric, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_resource_and_date">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.resource_id && doc.date) {\n emit([doc.project_id, doc.resource_id, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_metric_and_date">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.metric && doc.date) {\n emit([doc.project_id, doc.metric, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_date">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.date) {\n emit([doc.project_id, doc.date], doc);\n}\n}">>
         }]}
       },{<<"find_by_resource">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.resource_id) {\n emit([doc.project_id, doc.resource_id], doc);\n}\n}">>
         }]}
       },{<<"find_by_metric">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.metric) {\n emit([doc.project_id, doc.metric], doc);\n}\n}">>
         }]}
       },{<<"find_by_ID">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id) {\n emit(doc.project_id, doc);\n}\n}">>
         }]}
       },{<<"find_by_resource_and_metric">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.resource_id && doc.metric) {\n emit([doc.project_id, doc.resource_id, doc.metric], doc);\n}\n}">>
         }]}
       },{<<"metric_list_for_id">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.project_id && doc.metric) {\n emit([doc.project_id, doc.metric], null);\n}\n}">>
         }, {<<"reduce">>,
           <<"function (key, values, rereduce) {\n return null;\n }">>
         }]}
       },{<<"metric_list">>,
         {[{<<"map">>,
           <<"function (doc) {\n if (doc.metric) {\n emit(doc.metric, null);\n}\n}">>
         }, {<<"reduce">>,
           <<"function (key, values, rereduce) {\n return null;\n }">>
         }]}
       }]}
    }
  ]},
  {ok, _DesignDoc1} = couchbeam:save_doc(Database, Views),
  {ok, Database}.

do_map_reduce(ViewName, Options, Config) ->
  #ebilldb{ database = Database } = Config,
  OptionsDict = dict:from_list(Options),
  OptionsDict1 = case dict:is_key(options, OptionsDict) of
    true -> OptionsDict;
    false -> dict:append(options, [], OptionsDict)
  end,
  case dict:is_key(extract, OptionsDict1) of
    true -> do_extract_map_reduce(
          dict:fetch(extract, OptionsDict1),
          couchbeam_view:fetch(Database, {"ebill", ViewName}, dict:fetch(options, OptionsDict1))
        );
    false -> couchbeam_view:fetch(Database, {"ebill", ViewName}, dict:fetch(options, OptionsDict1))
  end.

do_extract_map_reduce(Fun, Result) ->
  case Result of
    {ok, []} -> [];
    {ok, Data} -> lists:foldl(Fun, [], Data);
    _ -> []
  end.

do_parse_map_reduce(Result) ->
  case Result of
    {ok, []} -> [];
    {ok, Data} -> 
      lists:foldl(fun({E}, Acc) ->
            case lists:keyfind(<<"value">>, 1, E) of
              {<<"value">>, {V}} -> 
                V1 = lists:keydelete(<<"_id">>, 1, V),
                V2 = lists:keydelete(<<"_rev">>, 1, V1),
                Acc ++ [V2];
              _ -> Acc
            end
        end, [], Data);
    _ -> []
  end.

do_date_interval(FixedTablePart, DateInterval) ->
  DIDict = ec_dict:from_list(DateInterval),
  StartDate = try ec_dict:get(start_date, DIDict) of
    StartDateValue -> list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s", ec_date:parse(bitstring_to_list(StartDateValue))))
  catch
    _ -> <<"1900-01-01T00:00:00">>
  end,
  EndDate = try ec_dict:get(end_date, DIDict) of
    EndDateValue -> list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s", ec_date:parse(bitstring_to_list(EndDateValue))))
  catch
    _ -> list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s",calendar:now_to_local_time(now())))
  end,
  {FixedTablePart ++ [StartDate], FixedTablePart ++ [EndDate]}.

find(Query) ->
  ID = case lists:keyfind(<<"project_id">>, 1, Query) of 
    {<<"project_id">>, I} -> I;
    _ -> <<"">>
  end,
  Datas = [],
  Datas1 = case lists:keyfind(<<"resource_id">>, 1, Query) of
    {<<"resource_id">>, R} -> Datas ++ [{resource, R}];
    _ -> Datas
  end,
  Datas2 = case lists:keyfind(<<"period">>, 1, Query) of
    {<<"period">>, P} -> Datas1 ++ [{dates, lists:map(fun({K, V}) -> {binary_to_atom(K, utf8), V} end, P)}];
    _ -> Datas1
  end,
  UnFilteredDatas = case lists:keyfind(<<"metrics">>, 1, Query) of
    {<<"metrics">>, MM} when is_list(MM) -> 
      lists:foldl(fun(M, Acc) ->
            Acc ++ find_by(ID, Datas2 ++ [{metric, binary_to_atom(M, utf8)}])
        end, [], MM);
    {<<"metrics">>, M} when is_atom(M) -> find_by(ID, Datas2 ++ [{metric, binary_to_atom(M, utf8)}]);
    _ -> find_by(ID, Datas2)
  end,
  %% TODO filter
  UnFilteredDatas.

