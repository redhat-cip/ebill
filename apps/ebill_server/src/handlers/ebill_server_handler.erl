-module(ebill_server_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([
  ebill_server_html/2,
  ebill_server_json/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
      {{<<"text">>, <<"html">>, '*'}, ebill_server_html},
      {{<<"application">>, <<"json">>, '*'}, ebill_server_json}
  ], Req, State}.

ebill_server_html(Req, State) ->
  Index = filename:join([code:priv_dir(ebill_server), "browser", "index.html"]),
  Body = list_to_bitstring(ebill_utils:readlines(Index)),
  {Body, Req, State}.

ebill_server_json(Req, State) ->
  Body = <<"
{
  \"title\": \"eBill Server\", 
  \"logo\": \"/static/enovance.png\"
}
  ">>,
  {Body, Req, State}.

