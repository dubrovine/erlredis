-module(redis_lua).
-include("redis.hrl").

-export([load_scripts/1]).
-export([q/2, q/3]).


% redis_lua:load_scripts(code:priv_dir(?APP_NAME) ++ "/lua/").
load_scripts(Dir)->
    SettingsList = 
    filelib:fold_files(Dir, "", true, fun(File, Acc)-> 
        Name = list_to_atom(filename:basename(File, ".lua")),
        {ok, LuaCode} = file:read_file(File),  
        Sha1 = list_to_binary(hexstring(crypto:hash(sha, LuaCode))),
        [{Name, {Sha1, LuaCode}} | Acc]
    end, []),
    ?LOG_INFO("[LUA] loaded", []),
    redis_settings:lua_set(SettingsList),
    ok.



q(FuncName, Query)->
    q(?REDIS_DEFAULT_POOL_NAME, FuncName, Query).
q(PoolName, FuncName, Query)->
    {Sha1, LuaCode} = redis_settings:lua_get(FuncName),

    Result = redis:q(PoolName, ["EVALSHA", Sha1 | Query]),
    case Result of
        {error, <<"NOSCRIPT", _/binary>>}  ->
            redis:q(PoolName, ["EVAL", LuaCode | Query]);
        _  ->
            Result
    end.




%%%%%%%%%%%%%%%% LOCAL




hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).
