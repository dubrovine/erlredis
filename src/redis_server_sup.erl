-module(redis_server_sup).
-behaviour(supervisor).

%% Supervisor.
-export([start_link/6]).
-export([init/1]).


start_link(PoolName, PoolType, PoolSize, InitSettings, DataBase, Password) ->
	SupName = list_to_atom(atom_to_list(PoolName) ++ "_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, [PoolName, PoolType, PoolSize, InitSettings, DataBase, Password]).


init([PoolName, PoolType, PoolSize, InitSettings, DataBase, Password]) ->
    Procs = [{redis_server,{redis_server, start_link, 
    	[PoolName, PoolType, PoolSize, InitSettings, DataBase, Password]}, 
    	permanent, 5000, worker, [dynamic]}],
    {ok, {{one_for_one, 100, 1}, Procs}}.
