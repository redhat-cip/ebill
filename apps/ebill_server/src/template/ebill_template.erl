-module(ebill_template).

-export([
  list/0,
  exist/1,
  delete/1,
  write/2,
  execute/2,
  execute/3,
  get_path/1,
  get_script_content/1
]).

list() ->
  [
    {ruby, list_with_ext("rb")},
    {python, list_with_ext("py")},
%     {lua, list_with_ext("lua")},
    {javascript, list_with_ext("js")}
  ].

list_with_ext(Ext) ->
  ScriptDir = code:priv_dir(ebill_server),
  ScriptMatch = filename:join([ScriptDir, "templates", "*." ++ Ext]),
  [list_to_binary(filename:basename(E, filename:extension(E))) 
   || E <- filelib:wildcard(ScriptMatch)].

exist(Name) ->
  ScriptDir = code:priv_dir(ebill_server),
  ScriptMatch = filename:join([ScriptDir, "templates", Name ++ ".*"]),
  case length(filelib:wildcard(ScriptMatch)) of
    0 -> false;
    _ -> true
  end.

delete(Name) ->
  ScriptDir = code:priv_dir(ebill_server),
  ScriptMatch = filename:join([ScriptDir, "templates", Name ++ ".*"]),
  case filelib:wildcard(ScriptMatch) of
    [ScriptFile|_] -> file:delete(ScriptFile);
    _ -> {error, list_to_binary("Can't find template " ++ Name)}
  end.

write(Filename, Content) ->
  TemplateName = filename:basename(Filename, filename:extension(Filename)),
  case exist(TemplateName) of
    true -> 
      {
        error, 
        list_to_binary("Template with name " ++ TemplateName ++ " already exist")
      };
    false ->
      ScriptDir = code:priv_dir(ebill_server),
      ScriptPath = filename:join([ScriptDir, "templates", Filename]),
      {ok, IoDevice} = file:open(ScriptPath, [raw, write, binary]),
      file:write(IoDevice, Content),
      file:close(IoDevice), 
      {
        template, 
        list_to_binary(TemplateName)
      }
  end.

execute(Script, Data) ->
  execute(Script, rate, Data).
execute(Script, Fun, Data) ->
  case get_path(Script) of
    {ok, ScriptFile} ->
      [_|ExtWithoutDot] = string:to_lower(filename:extension(ScriptFile)),
      case ExtWithoutDot of
        "rb" -> execute_ruby_template(ScriptFile, Fun, Data);
        "py" -> execute_python_template(ScriptFile, Fun, Data);
%         "lua" -> execute_lua_template(ScriptFile, Fun, Data);
        "js" -> execute_js_template(ScriptFile, Fun, Data);
        _ -> {error, unsupported_template}
      end;
    _ -> {error, template_not_found}
  end.

get_path(Script) ->
  ScriptDir = code:priv_dir(ebill_server),
  ScriptMatch = filename:join([ScriptDir, "templates", Script ++ ".*"]),
  case filelib:wildcard(ScriptMatch) of
    [ScriptFile|_] -> {ok, ScriptFile};
    _ -> {error, not_found}
  end.

get_script_content(Script) ->
  case get_path(Script) of
    {ok, ScriptFile} -> {ok, ebill_utils:readlines(ScriptFile)};
    _ -> {errror, template_not_found}
  end.

execute_ruby_template(ScriptFile, Fun, Data) ->
  ModulePath = filename:dirname(ScriptFile),
  HelperPath = filename:join([ModulePath, "helpers", "ruby"]),
  ModuleName = filename:basename(ScriptFile, ".rb"),
  {ok, R} = ruby:start([{ruby_lib, [ModulePath, HelperPath]}]),
  CallResult = ruby:call(R, list_to_atom(ModuleName), Fun, [Data]),
  ruby:stop(R),
  [{Code, JSONResult}|_] = jsx:decode(CallResult),
  {binary_to_atom(Code, utf8), JSONResult}.

execute_python_template(ScriptFile, Fun, Data) ->
  ModulePath = filename:dirname(ScriptFile),
  HelperPath = filename:join([ModulePath, "helpers", "python"]),
  ModuleName = filename:basename(ScriptFile, ".py"),
  {ok, R} = python:start([{python_path, [ModulePath, HelperPath]}]),
  CallResult = python:call(R, list_to_atom(ModuleName), Fun, [Data]),
  python:stop(R),
  [{Code, JSONResult}|_] = jsx:decode(CallResult),
  {binary_to_atom(Code, utf8), JSONResult}.

% execute_lua_template(ScriptFile, Fun, Data) ->
%   {ok, ScriptData} = file:read_file(ScriptFile),
%   {ok, L} = lua:new_state(),
%   ok = lual:dostring(L, ScriptData),
%   CallResult = luam:call(L, atom_to_list(Fun), [Data]),
%   [{Code, JSONResult}|_] = jsx:decode(CallResult),
%   {binary_to_atom(Code, utf8), JSONResult}.

execute_js_template(ScriptFile, Fun, Data) ->
  {ok, ScriptData} = file:read_file(ScriptFile),
  ModulePath = filename:dirname(ScriptFile),
  HelperPath = filename:join([ModulePath, "helpers", "javascript", "ebill.js"]),
  {ok, HelperData} = file:read_file(HelperPath),
  application:start(erlang_js),
  {ok, JS} = js_driver:new(),
  ok = js:define(JS, HelperData),
  ok = js:define(JS, ScriptData),
  {ok, CallResult} = js:call(JS, atom_to_binary(Fun, utf8), [Data]),
  application:stop(erlang_js),
  [{Code, JSONResult}|_] = jsx:decode(CallResult),
  {binary_to_atom(Code, utf8), JSONResult}.
