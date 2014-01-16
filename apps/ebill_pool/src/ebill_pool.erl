-module(ebill_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/pool.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, set_nodes/1, get_type/0, get_nodes/1]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  erlang:set_cookie(node(), ebill_config:get(cookie)),
  erlang:send_after(1000, self(), trigger),
  {ok, #pool{node = node()}}.

handle_call({set_nodes, Nodes}, _From, State) ->
  lager:info("nodes = ~p", [Nodes]),
  erlang:send_after(10000, self(), trigger),
  {reply, ok, State#pool{scanning = false, nodes = Nodes}};
handle_call({get_nodes, Type}, _From, #pool{nodes = Nodes} = State) ->
  NodesDict = dict:from_list(Nodes),
  R = case dict:is_key(Type, NodesDict) of
    true -> dict:fetch(Type, NodesDict);
    flase -> []
  end,
  {reply, R, State};
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
