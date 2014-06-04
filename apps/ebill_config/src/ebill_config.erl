%% @doc This module allow you to acces the ebill configuration
-module(ebill_config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/ebill.hrl").

-export([start_link/0, get/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% wrappers

%% @doc Start the configuration server
start_link() -> 
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Access a configuration information
get(tcp_server_port) ->
  gen_server:call(?SERVER, {get_tcp_server_port});
get(max_server_conn) ->
  gen_server:call(?SERVER, {get_max_server_conn});
get(tcp_storage_port) ->
  gen_server:call(?SERVER, {get_tcp_storage_port});
get(max_storage_conn) ->
  gen_server:call(?SERVER, {get_max_storage_conn});
get(cookie) ->
  gen_server:call(?SERVER, {get_cookie});
get(db_storage_host) ->
  gen_server:call(?SERVER, {get_db_storage_host});
get(db_storage_port) ->
  gen_server:call(?SERVER, {get_db_storage_port});
get(db_storage_name) ->
  gen_server:call(?SERVER, {get_db_storage_name});
get(_) ->
  {error, "Unknown config tag"}.

% Server

%% @hidden
init([]) ->
  {ok, read_config()}.

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

%% @hidden
handle_call({get_tcp_server_port}, _From, Config) ->
  #ebillconfig{tcp_server_port = TcpPort} = Config,
  {reply, TcpPort, Config};
handle_call({get_max_server_conn}, _From, Config) ->
  #ebillconfig{max_server_conn = MaxConn} = Config,
  {reply, MaxConn, Config};
handle_call({get_tcp_storage_port}, _From, Config) ->
  #ebillconfig{tcp_storage_port = TcpPort} = Config,
  {reply, TcpPort, Config};
handle_call({get_max_storage_conn}, _From, Config) ->
  #ebillconfig{max_storage_conn = MaxConn} = Config,
  {reply, MaxConn, Config};
handle_call({get_cookie}, _From, Config) ->
  #ebillconfig{cookie = Cookie} = Config,
  {reply, Cookie, Config};
handle_call({get_db_storage_host}, _From, Config) ->
  #ebillconfig{db_storage_host = DBHost} = Config,
  {reply, DBHost, Config};
handle_call({get_db_storage_port}, _From, Config) ->
  #ebillconfig{db_storage_port = DBPort} = Config,
  {reply, DBPort, Config};
handle_call({get_db_storage_name}, _From, Config) ->
  #ebillconfig{db_storage_name = DBName} = Config,
  {reply, DBName, Config};
handle_call(_Message, _From, Config) ->
  {reply, error, Config}.

% private

read_config() ->
  lager:info("Check configuration"),
  find_config(?EBILL_CONFIG_PATH, #ebillconfig{}).

find_config([], Config) ->
  Config;
find_config([F|H], Config) ->
  ConfFile = ebill_utils:expand_path(F),
  case filelib:is_file(ConfFile) of
    true -> 
      find_config(H, read_config_file(ConfFile, Config));
    false -> 
      find_config(H, Config)
  end.

read_config_file(ConfFile, Config) ->
  lager:info("Update configuration with ~p", [ConfFile]),
  IniBin = case file:read_file(ConfFile) of
    {ok, IniBin0} ->
      IniBin0;
    {error, eacces} ->
      throw({file_permission_error, ConfFile});
    {error, enoent} ->
      Fmt = "Couldn't find server configuration file ~s.",
      Msg = list_to_binary(io_lib:format(Fmt, [ConfFile])),
      throw({startup_error, Msg})
  end,
  Lines = re:split(IniBin, "\r\n|\n|\r|\032", [{return, list}]),
  Config1 = lists:foldl(fun(Line, Acc) ->
        case string:strip(Line) of
          "" -> 
            Acc;
          "#" ++ _Rest ->
            Acc;
          Rest ->
            case re:split(Rest, "\s*=\s*", [{return, list}]) of
              [Key|Value] ->
                Value1 = string:join(Value, "="),
                Value2 = case re:split(Value1, "\s*#", [{return, list}]) of
                  [Value3] -> Value3;
                  [Value4|_] -> Value4
                end,
                case list_to_atom(Key) of
                  tcp_server_port ->
                    Acc#ebillconfig{tcp_server_port = list_to_integer(Value2)};
                  max_server_conn ->
                    Acc#ebillconfig{max_server_conn = list_to_integer(Value2)};
                  tcp_storage_port ->
                    Acc#ebillconfig{tcp_storage_port = list_to_integer(Value2)};
                  max_storage_conn ->
                    Acc#ebillconfig{max_storage_conn = list_to_integer(Value2)};
                  cookie ->
                    Acc#ebillconfig{cookie = list_to_atom(Value2)};
                  db_storage_host ->
                    Acc#ebillconfig{db_storage_host = Value2};
                  db_storage_port ->
                    Acc#ebillconfig{db_storage_port = list_to_integer(Value2)};
                  db_storage_name ->
                    Acc#ebillconfig{db_storage_name = Value2};
                  Other ->
                    lager:info("Unknow option `~p' : ignored", [Other]),
                    Acc
                end;
              _ ->
                Acc
            end
        end
    end, Config, Lines),
  Config1.

