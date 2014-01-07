-module(ebill_storage_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([
  ebill_storage_html/2,
  ebill_storage_json/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
      {<<"text/html">>, ebill_storage_html},
      {<<"application/json">>, ebill_storage_json}
  ], Req, State}.

ebill_storage_html(Req, State) ->
  Body = <<"
<html>
  <body>
    <h1>eBill Storage</h1>
    <img src='/static/enovance.png' /> 
  </body>
</html>
  ">>,
  {Body, Req, State}.

ebill_storage_json(Req, State) ->
  Body = <<"
{
  \"title\": \"eBill Storage\", 
  \"logo\": \"/static/enovance.png\"
}
  ">>,
  {Body, Req, State}.
