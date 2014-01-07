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
      {<<"text/html">>, ebill_server_html},
      {<<"application/json">>, ebill_server_json}
  ], Req, State}.

ebill_server_html(Req, State) ->
  Body = <<"
<html>
  <body>
    <h1>eBill Server</h1>
    <img src='/static/enovance.png' /> 
  </body>
</html>
  ">>,
  {Body, Req, State}.

ebill_server_json(Req, State) ->
  Body = <<"
{
  \"title\": \"eBill Server\", 
  \"logo\": \"/static/enovance.png\"
}
  ">>,
  {Body, Req, State}.
