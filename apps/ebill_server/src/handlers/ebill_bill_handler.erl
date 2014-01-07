-module(ebill_bill_handler).

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
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {<<"application/json">>, from_json},
      {<<"text/plain">>, from_text}
  ], Req, State}.

accept_event_body(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[
      {<<"text/html">>, to_html},
      {<<"application/json">>, to_json}
  ], Req, State}.

from_json(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  Body = "from_json : " ++ binary_to_list(Data),
  Req2 = cowboy_req:set_resp_body(Body, Req),
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
