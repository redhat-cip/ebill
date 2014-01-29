-module(ebill_template_handler).

-export([
  init/3,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2
  ]).
-export([
  do_post/2,
  do_get_head/2,
  delete_resource/2,
  resource_exists/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {{<<"multipart">>, <<"form-data">>, '*'}, do_post}
  ], Req, State}.

content_types_provided(Req, State) ->
  {[
      {{<<"application">>, <<"json">>, '*'}, do_get_head}
  ], Req, State}.

do_get_head(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"HEAD">>, Req2} -> do_head(Req2, State);
    {<<"GET">>, Req2} -> do_get(Req2, State)
  end.

% POST /template
do_post(Req, State) ->
  {Result, Req2} = acc_multipart(Req),
  Data = proplists:get_value(body, Result),
  {<<"form-data">>, FormData} = cowboy_multipart:content_disposition(
    proplists:get_value(<<"content-disposition">>, proplists:get_value(header, Result))
  ),
  Filename = proplists:get_value(<<"filename">>, FormData),
  Body = ebill_template:write(binary_to_list(Filename), binary_to_list(Data)),
  Body2 = jsx:encode([Body]),
  Req3 = cowboy_req:set_resp_body(Body2, Req2),
  {true, Req3, State}.

acc_multipart(Req) ->
  acc_multipart(cowboy_req:multipart_data(Req), []).
acc_multipart({headers, Headers, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), Acc ++ [{header, Headers}]);
acc_multipart({body, Data, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), Acc ++ [{body, Data}]);
acc_multipart({end_of_part, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), Acc);
acc_multipart({eof, Req}, Acc) ->
  {Acc, Req}.

% GET /template/:id
do_get(Req, State) ->
  case cowboy_req:bindings(Req) of
    {[], Req2} -> 
      do_get_all(Req2, State);
    {Bindings, Req2} -> 
      Template = binary_to_list(proplists:get_value(id, Bindings)),
      do_get_one(Template, Req2, State)
  end.

do_get_all(Req, State) ->
  Templates = ebill_template:list(),
  Body = jsx:encode(Templates),
  {Body, Req, State}.

do_get_one(Template, Req, State) ->
  TemplateScriptInfo = case ebill_template:get_script_content(Template) of
    {ok, ScriptContent} -> {script, list_to_binary(ScriptContent)};
    _ -> {error, <<"Template not found">>}
  end,
  Body = jsx:encode([
    {template, list_to_binary(Template)},
    TemplateScriptInfo 
  ]),
  {Body, Req, State}.

% HEAD /template/:id
do_head(Req, State) ->
  {"", Req, State}.

% DELETE /template/:id
delete_resource(Req, State) ->
  {Bindings, Req2} = cowboy_req:bindings(Req),
  Template = binary_to_list(proplists:get_value(id, Bindings)),
  case ebill_template:delete(Template) of
    ok -> 
      {true, Req2, State};
    Body ->
      Body2 = jsx:encode([Body]),
      Req3 = cowboy_req:set_resp_body(Body2, Req2),
      {true, Req3, State}
  end.

resource_exists(Req, State) ->
  case cowboy_req:bindings(Req) of
    {[], Req2} -> 
      case cowboy_req:method(Req2) of
        {<<"GET">>, Req3} ->
          {true, Req3, State};
        {_, Req3} ->
          {false, Req3, State}
      end;
    {Bindings, Req2} -> 
      Template = binary_to_list(proplists:get_value(id, Bindings)),
      {ebill_template:exist(Template), Req2, State}
  end.
