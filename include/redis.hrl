-define(LOG_INFO(LogMsg, LogArgs), logger:info(LogMsg, LogArgs)).
-define(LOG_ERROR(LogMsg, LogArgs),  logger:error(LogMsg, LogArgs)).

-define(REDIS_ETS_INFO, redis_ets_info).
-define(REDIS_DEFAULT_POOL_NAME, redis_default_pool).

-define(REDIS_TYPE_CLUSTER, cluster).
-define(REDIS_TYPE_SENTINEL, sentinel).
-define(REDIS_TYPE_SINGLE, single).

-record(state, {
    type,
    size,
    name,
    database,
    password,
    init_settings,
    last_reload_time = 0,
    % cluster
    cluster_version = 0,
    cluster_nodes = [],
    % sentinel
    sentinel_host,
    sentinel_port
}).
-record(cluster_pools, {
    pools = [],
    version = 0
}).
-record(cluster_pool, {
    name,
    s_start,
    s_end
}).