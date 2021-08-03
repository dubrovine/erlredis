-module(redis_server).

-behaviour(gen_server).
-include("redis.hrl").

% API
-export([try_reload/3]).
-export([reload/2]).
%% gen_server.
-export([start_link/6]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%%%%%%%%%%%%%%%%%%% API

try_reload(PoolType, PoolName, Version)->
    case PoolType of
        ?REDIS_TYPE_CLUSTER ->  
            gen_server:call(serv_name(PoolName), {try_reload, Version});
        ?REDIS_TYPE_SENTINEL ->  
            gen_server:call(serv_name(PoolName), {try_reload, Version});
        ?REDIS_TYPE_SINGLE -> 
            ok
    end.



reload(PoolName, Version)->
    gen_server:call(serv_name(PoolName), {reload, Version}).



start_link(PoolName, PoolType, PoolSize, InitSettings, DataBase, Password) ->   
    gen_server:start_link({local, serv_name(PoolName)}, ?MODULE, [PoolName, PoolType, PoolSize, InitSettings, DataBase, Password], []).



%%%%%%%%%%%%%%%%%%% gen_server.



init([PoolName, PoolType, PoolSize, InitSettings, DataBase, Password]) ->

    redis_settings:pool_type_set(PoolName, PoolType),

    State = start_pools(#state{
        type = PoolType, 
        name = PoolName,
        size = PoolSize,
        database =  DataBase,
        password = Password,
        init_settings = InitSettings,
        last_reload_time = erlang:system_time(seconds)
    }),
    
    {ok, State}.


% reload cluster ignore
handle_call({reload, Version}, _From, #state{type = ?REDIS_TYPE_CLUSTER, cluster_version=Version2} = State) when Version =/= Version2 ->    
    {reply, ignore, State};
handle_call({try_reload, Version}, _From, #state{type = ?REDIS_TYPE_CLUSTER, cluster_version=Version2} = State) when Version =/= Version2 ->    
    {reply, ignore, State};


% reload
handle_call({reload, _Version}, _From, State) ->  
    {reply, ok, reload_pools(State#state{
        last_reload_time = erlang:system_time(seconds)
    })};

% try_reload
handle_call({try_reload, _Version}, _From, #state{last_reload_time=LastTime} = State) ->    
    case (erlang:system_time(seconds) > (LastTime + 1)) of
        true  -> 
                {reply, ok, reload_pools(State#state{
                    last_reload_time = erlang:system_time(seconds)
                })};
        false -> 
                {reply, ignore, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%%%%%%%%%% LOCAL


start_pools(State = #state{type = ?REDIS_TYPE_CLUSTER})->
    redis_settings:cluster_pools_set(State#state.name, #cluster_pools{}),
    NewState = 
    redis_cluster:reload_pools(State#state{
        cluster_nodes = State#state.init_settings
    }),
    NewState;

start_pools(State = #state{type = ?REDIS_TYPE_SENTINEL})->
    NewState = redis_sentinel:reload_pools(State),
    NewState;

start_pools(State = #state{type = ?REDIS_TYPE_SINGLE})->
    {Host, Port} = State#state.init_settings,
    erlpool:start_pool(State#state.name, [
        {size, State#state.size},
        {group, redis_pool},
        {start_mfa, {eredis, start_link, [Host, Port, State#state.database, State#state.password]}}
    ]),
    State.




reload_pools(State = #state{type = ?REDIS_TYPE_CLUSTER})->
    ?LOG_INFO("[REDIS] SERVER_RELOAD  ~p", [State#state.cluster_version]),
    try
        redis_cluster:reload_pools(State)
    catch T:R:S->
        ?LOG_ERROR("[REDIS] SERVER_ERR ~p ~p ~p", [T, R, S]),
        State
    end;

reload_pools(State = #state{type = ?REDIS_TYPE_SENTINEL})->
    ?LOG_INFO("[REDIS] SERVER_RELOAD", []),
    NewState = redis_sentinel:reload_pools(State),
    NewState.


serv_name(PoolName)->
    list_to_atom(atom_to_list(PoolName) ++ "_serv").
