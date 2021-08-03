-module(redis_settings).
-include("redis.hrl").

-export([lua_set/1]).
-export([lua_get/1]).
-export([pool_type_get/1]).
-export([pool_type_set/2]).
-export([cluster_pools_get/1]).
-export([cluster_pools_set/2]).


lua_get(Key)->
    persistent_term:get({?MODULE, Key}, undefined).
lua_set(List)-> 
    lists:foreach(fun({Key, Value})-> 
        persistent_term:put({?MODULE, Key}, Value)
    end, List). 


pool_type_get(PoolName)->
    persistent_term:get({?MODULE, PoolName}, undefined).

pool_type_set(PoolName, Type)->
    persistent_term:put({?MODULE, PoolName}, Type).




cluster_pools_get(PoolName)->
	persistent_term:get({PoolName, pools}, []).

cluster_pools_set(PoolName, Pools)->
	persistent_term:put({PoolName, pools}, Pools).