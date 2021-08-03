# erlredis

Redis client over `eredis` and `erlpool`.

Supported features:
 
 * Redis Cluser with auto reshard
 * Redis Sentinel with auto master detect
 * Redis Single node
 * Redis lua helper


## Redis Load Lua scripts
```erlang
% start
ok = redis_lua:load_scripts(code:priv_dir(?APP_NAME) ++ "/lua/").
```



## Redis Cluser
```erlang
% start
{ok, Pid} = redis:start_cluster(PoolSize, InitHosts, DataBase, Password).

% start with poolName
{ok, Pid} = redis:start_cluster(PoolName, PoolSize, InitHosts, DataBase, Password).

% start Example
{ok, Pid} = redis:start_cluster(pool1, 10, [{"127.0.0.1", 6379}], 0, "mypassword").

% query
{ok, <<"val">>} = redis:q(["GET", "test"]).

% query with poolName
{ok, <<"val">>} = redis:q(pool1, ["GET", "test"]).

% query lua
{ok, <<"val">>} = redis_lua:q(lua_file_name, LuaParams).

% query lua with poolName
{ok, <<"val">>} = redis_lua:q(pool1, lua_file_name, LuaParams).
```

## Redis Sentinel
```erlang
% start
{ok, Pid} = redis:start_sentinel(PoolSize, InitHosts, MasterName, DataBase, Password).

% start with poolName
{ok, Pid} = redis:start_sentinel(PoolName, PoolSize, InitHosts, MasterName, DataBase, Password).

% start Example
{ok, Pid} = redis:start_sentinel(pool1, 10, [{"127.0.0.1", 6379}], "myMasterName", 0, "mypassword").

% query
{ok, <<"val">>} = redis:q(["GET", "test"]).

% query with poolName
{ok, <<"val">>} = redis:q(pool1, ["GET", "test"]).

% query lua
{ok, <<"val">>} = redis_lua:q(lua_file_name, LuaParams).

% query lua with poolName
{ok, <<"val">>} = redis_lua:q(pool1, lua_file_name, LuaParams).
```


## Redis single node
```erlang
% start
{ok, Pid} = redis:start_single(PoolSize, Host, Port, DataBase, Password).

% start with poolName
{ok, Pid} = redis:start_single(PoolName, PoolSize, Host, Port, DataBase, Password).

% start Example
{ok, Pid} = redis:start_single(pool1, 10, "127.0.0.1", 6379, 0, "mypassword").

% query
{ok, <<"val">>} = redis:q(["GET", "test"]).

% query with poolName
{ok, <<"val">>} = redis:q(pool1, ["GET", "test"]).

% query lua
{ok, <<"val">>} = redis_lua:q(lua_file_name, LuaParams).

% query lua with poolName
{ok, <<"val">>} = redis_lua:q(pool1, lua_file_name, LuaParams).
```
# erlredis
