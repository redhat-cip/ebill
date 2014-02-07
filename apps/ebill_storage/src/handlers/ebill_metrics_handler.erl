-module(ebill_metrics_handler).

-export([
  init/3,
  allowed_methods/2,
  content_types_accepted/2,
  accept_event_body/2,
  content_types_provided/2
  ]).
-export([
  from_json/2,
  from_text/2,
  to_html/2,
  to_json/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {{<<"application">>, <<"json">>, '*'}, from_json},
      {{<<"text">>, <<"plain">>, '*'}, from_text}
  ], Req, State}.

accept_event_body(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[
      {{<<"text">>, <<"html">>, '*'}, to_html},
      {{<<"application">>, <<"json">>, '*'}, to_json}
  ], Req, State}.

from_json(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  Req2 = case jsx:is_json(Data) of
    true -> insert_data(jsx:decode(Data), Req);
    false -> cowboy_req:reply(400, Req)
  end,
  {true, Req2, State}.

from_text(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  Body = "from_text : " ++ binary_to_list(Data),
  Req2 = cowboy_req:set_resp_body(Body, Req),
  {true, Req2, State}.

to_html(Req, State) ->
  Body = "tohtml",
  {Body, Req, State}.

to_json(Req, State) ->
  Body = "tojson",
  {Body, Req, State}.

% Private

insert_data(JSON, Req) ->
  JSONDict = dict:from_list(JSON),
  case lists:foldl(fun(E, Acc) ->
        Acc and dict:is_key(E, JSONDict)
      end, true, [<<"project_id">>, <<"resource_id">>, <<"metrics">>]) and is_list(dict:fetch(<<"metrics">>, JSONDict)) of
    true ->
      ID = dict:fetch(<<"project_id">>, JSONDict),
      Resource = dict:fetch(<<"resource_id">>, JSONDict),
      Date = case dict:is_key(<<"date">>, JSONDict) of
        true -> dict:fetch(<<"date">>, JSONDict);
        false -> list_to_bitstring(ec_date:format("Y-m-d\\TH:i:s",calendar:now_to_local_time(now())))
      end,
      Metadatas = case dict:is_key(<<"metadatas">>, JSONDict) of
        true -> dict:fetch(<<"metadatas">>, JSONDict);
        false -> []
      end,
      lists:foreach(fun({Metric, Value}) ->
            Metric1 = binary_to_atom(Metric, utf8),
            ebill_data:add(ID, Resource, Metric1, Value, Metadatas, Date)
        end, dict:fetch(<<"metrics">>, JSONDict)),
      Body = "from_json : " ++ bitstring_to_list(jsx:encode(JSON)),
      cowboy_req:set_resp_body(Body, Req);
    false ->
      cowboy_req:reply(400, Req)
  end.

