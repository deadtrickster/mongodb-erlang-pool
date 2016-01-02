-module(mongodb_pool_sup).

-behaviour(supervisor).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, start_link/2]).
-export([create_pool/3, create_pool/4, delete_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  case application:get_env(mongodb_pool, pools) of
    {ok, Pools} -> 
      {ok, GlobalOrLocal} = application:get_env(mongodb_pool, global_or_local),
      start_link(Pools, GlobalOrLocal);
    _ ->
      supervisor:start_link({local, ?MODULE}, ?MODULE, [[], local])
  end.

start_link(Pools, GlobalOrLocal) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools, GlobalOrLocal]).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer(), Options::[tuple()]) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Options) ->
    create_pool(local, PoolName, Size, Options).

%% ===================================================================
%% @doc create new pool, selectable name zone global or local.
%% @end
%% ===================================================================
-spec(create_pool(GlobalOrLocal::atom(), PoolName::atom(), Size::integer(), Options::[tuple()]) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(GlobalOrLocal, PoolName, Size, Options) 
  when GlobalOrLocal =:= local;
       GlobalOrLocal =:= global ->

    PoolArgs = [{name, {GlobalOrLocal, PoolName}}, {worker_module, mc_worker}],
    PoolSpec = poolboy:child_spec(PoolName, PoolArgs ++ Size, Options),

    supervisor:start_child(?MODULE, PoolSpec).

%% ===================================================================
%% @doc delete pool
%% @end
%% ===================================================================
-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).

delete_pool(PoolName) ->
    supervisor:terminate_child(?MODULE, PoolName),
    supervisor:delete_child(?MODULE, PoolName).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Pools, GlobalOrLocal]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {GlobalOrLocal, Name}},
                    {worker_module, mc_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {SupFlags, PoolSpecs}}.
