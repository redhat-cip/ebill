-module(ebill_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/pool.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  start_link/0, 
  set_nodes/1, 
  get_type/0, 
  get_nodes/1,
  call/4
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_nodes(Nodes) ->
  gen_server:call(?MODULE, {set_nodes, Nodes}).

get_type() ->
  application:get_env(ebill_pool, type).

get_nodes(Type) ->
  gen_server:call(?MODULE, {get_nodes, Type}).

call(Type, Module, Function, Args) ->
  try gen_server:call(?MODULE, {call, Type, Module, Function, Args})
  catch
    exit:{timeout,_} -> {error, timeout}
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  erlang:set_cookie(node(), ebill_config:get(cookie)),
  erlang:send_after(1000, self(), trigger),
  {ok, #pool{node = node()}}.

handle_call({set_nodes, Nodes}, _From, #pool{counters = Counters} = State) ->
  %% Remove deprecated counters
  Counters1 = lists:filter(fun({K, _}) ->
        case lists:keyfind(K, 1, Nodes) of
          false -> false;
          _ -> true
        end
    end, Counters),
  %% Add new counters
  Counters2 = lists:foldl(fun({K, _}, C) ->
        case lists:keyfind(K, 1, Counters1) of
          false -> C ++ [{K, 0}];
          _ -> C
        end
    end, Counters1, Nodes),

  lager:info("nodes = ~p / counters = ~p", [Nodes, Counters2]),
  erlang:send_after(10000, self(), trigger),
  {reply, ok, State#pool{scanning = false, nodes = Nodes, counters = Counters2}};
handle_call({get_nodes, Type}, _From, #pool{nodes = Nodes} = State) ->
  R = case lists:keyfind(Type, 1, Nodes) of
    false -> [];
    {Type, L} -> L
  end,
  {reply, R, State};
handle_call({call, Type, Module, Function, Args}, _From, #pool{nodes = Nodes, counters = Counters} = State) ->
  NodesForType = case lists:keyfind(Type, 1, Nodes) of
    false -> [];
    {Type, L} -> L
  end,
  Counter = case lists:keyfind(Type, 1, Counters) of
    false -> 0;
    {Type, C} -> C
  end,
  RRNode = case length(NodesForType) of
    0 -> error;
    1 -> [N|_] = NodesForType, N;
    _ -> lists:nth((Counter rem length(NodesForType)), NodesForType)
  end,
  case RRNode of
    error -> {reply, {error, notfound}, State};
    Node -> 
      Counters1 = lists:keyreplace(Type, 1, Counters, {Type, Counter + 1}),
      case rpc:call(Node, Module, Function, Args) of 
        {badrpc,nodedown} -> {reply, {error, nodedown}, State#pool{counters = Counters1}};
        R -> {reply, {ok, R}, State#pool{counters = Counters1}}
      end
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, #pool{scanning = Scanning} = State) ->
  State1 = case Scanning of
    true -> Scanning;
    false -> start_scanning(State)
  end,
  {noreply, State1}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_scanning(State) ->
  lager:info("Start scanning..."),
  State1 = State#pool{scanning = true},
  spawn_link(fun run_scan/0),
  State1.

run_scan() ->
  Nodes = lists:foldl(fun(N, Acc) ->
        case rpc:call(N, ebill_pool, get_type, []) of
          {badrpc, _} -> Acc;
          {ok, Res} -> 
            D = dict:from_list(Acc),
            D1 = dict:append(Res, N, D),
            dict:to_list(D1)
        end
    end, [], net_adm:world()),
  ebill_pool:set_nodes(Nodes).
