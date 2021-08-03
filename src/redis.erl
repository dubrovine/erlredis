-module(redis).
-include("redis.hrl").

% API
-export([start_cluster/4, start_cluster/5]).
-export([start_sentinel/5, start_sentinel/6]).
-export([start_single/5, start_single/6]).
% API query
-export([q/1, q/2]).



% start cluster pool
start_cluster(PoolSize, InitHosts, DataBase, Password)->
    start_cluster(?REDIS_DEFAULT_POOL_NAME, PoolSize, InitHosts, DataBase, Password).
start_cluster(PoolName, PoolSize, InitHosts, DataBase, Password)->
    redis_server_sup:start_link(PoolName, ?REDIS_TYPE_CLUSTER, PoolSize, InitHosts, DataBase, Password).


% start sentinel pool
start_sentinel(PoolSize, InitHosts, MasterName, DataBase, Password)->
    start_sentinel(?REDIS_DEFAULT_POOL_NAME, PoolSize, InitHosts, MasterName, DataBase, Password).
start_sentinel(PoolName, PoolSize, InitHosts, MasterName, DataBase, Password)->
    redis_server_sup:start_link(PoolName, ?REDIS_TYPE_SENTINEL, PoolSize, {InitHosts, MasterName}, DataBase, Password).


% start single pool
start_single(PoolSize, Host, Port, DataBase, Password)->
    start_single(?REDIS_DEFAULT_POOL_NAME, PoolSize, Host, Port, DataBase, Password).
start_single(PoolName, PoolSize, Host, Port, DataBase, Password)->
    redis_server_sup:start_link(PoolName, ?REDIS_TYPE_SINGLE, PoolSize, {Host, Port}, DataBase, Password).



% query to redis pool
q(Query)-> 
    q(?REDIS_DEFAULT_POOL_NAME, Query).
q(PoolName, Query)-> 
    PoolType = redis_settings:pool_type_get(PoolName),
    run_q(PoolType, PoolName, Query).



%%%%%%%%%%%%%%%%%%% LOCAL



run_q(PoolType, PoolName, Query)->
    run_q(PoolType, PoolName, Query, 0).

run_q(PoolType, PoolName, _, 15)->
    ?LOG_ERROR("[REDIS_ERR] limit_count_resend ~p ~p", [PoolType, PoolName]),
    {error, limit_count_resend};

run_q(PoolType, PoolName, Query, Counter)->

    {ServerPid, Version} = get_worker(PoolType, PoolName, Query),

    Result = 
    try
        case Query of
            [[X|_]|_] when is_list(X); is_binary(X) -> 
                eredis:qp(ServerPid, Query);
            _ -> 
                eredis:q(ServerPid, Query)
        end

    catch T:R:S ->
        ?LOG_ERROR("[REDIS_ERR] ~p ~p ~p", [T, R, S]),
        {error, to_error(R)}
    end,

    case Result of

        {error, tcp_closed} ->
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);
            
        {error, nodedown} ->
            redis_server:try_reload(PoolType, PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        {error, no_connection} ->
            redis_server:try_reload(PoolType, PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        {error, timeout} ->
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        % Redis explicitly say our slot mapping is incorrect,
        % we need to refresh it
        {error, <<"MOVED ", _/binary>>} ->
            ?LOG_ERROR("[REDIS_ERR] MOVED", []),
            redis_server:try_reload(PoolType, PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        {error, <<"CLUSTERDOWN ", _/binary>>} ->
            ?LOG_ERROR("[REDIS_ERR] CLUSTERDOWN", []),
            redis_server:reload(PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);


        {error, <<"READONLY", _/binary>>} ->
            ?LOG_ERROR("[REDIS_ERR] READONLY", []),
            redis_server:try_reload(PoolType, PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        {{nodedown,_},_} ->
            redis_server:try_reload(PoolType, PoolName, Version),
            timer:sleep(100),
            run_q(PoolType, PoolName, Query, Counter+1);

        _ ->
            Result
    end.




get_worker(PoolType, PoolName, Query)->
    case PoolType of
        ?REDIS_TYPE_CLUSTER ->
            {PoolNameBySlot, Version} = redis_cluster:worker(PoolName, Query),
            {erlpool:pid(PoolNameBySlot), Version};
        ?REDIS_TYPE_SENTINEL ->
            ServerPid = erlpool:pid(PoolName),
            {ServerPid, undefined};
        ?REDIS_TYPE_SINGLE ->
            ServerPid = erlpool:pid(PoolName),
            {ServerPid, undefined}
    end.



to_error({timeout, _}) ->
    timeout;
to_error(R) ->
    R.