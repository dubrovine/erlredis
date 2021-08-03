-module(redis_sentinel).
-include("redis.hrl").

%% API.
-export([reload_pools/1]).

-define(SOCKET_OPS, [binary, {packet, raw}, {active, false}]).


reload_pools(State)->
    {InitHosts, MasterName} = State#state.init_settings,
    Result = get_master(InitHosts, MasterName),
    case Result of
        {ok, {Host, Port}} ->
            try_start_pool(Host, Port, State),
            State#state{
                sentinel_host = Host,
                sentinel_port = Port
            };
        _ ->
        ?LOG_ERROR("[REDIS] error ~p", [Result]),
        State
    end.






%%%%%%%%%%%%%%%%%%%% LOCAL



% master not changed
% ignore
try_start_pool(Host, Port, State = #state{sentinel_host = Host, sentinel_port = Port})->
    ?LOG_INFO("[REDIS] SENTINEL NOT_CHANGED_POOL: ~p ~s:~p", [State#state.name, Host, Port]),
    ignore;
try_start_pool(Host, Port, State)->
    PoolRes = start_pool(Host, Port, State),
    case PoolRes of
        ok -> ok;
        {error,{already_started,_}} -> 
            ?LOG_INFO("[REDIS] SENTINEL STOP_POOL: ~p ~s:~p", [State#state.name, State#state.sentinel_host, State#state.sentinel_port]),
            catch erlpool:stop_pool(State#state.name),
            ok = start_pool(Host, Port, State);
        PoolErr ->
            exit(PoolErr)
    end.


start_pool(Host, Port, State)->    
    ?LOG_INFO("[REDIS] SENTINEL START_POOL: ~p ~s:~p", [State#state.name, Host, Port]),
    erlpool:start_pool(State#state.name, [
        {size, State#state.size},
        {group, redis_pool},
        {start_mfa, {eredis, start_link, [Host, Port, State#state.database, State#state.password]}}
    ]).





get_master([CurrentSentinel | OtherSentinels], MasterName) ->
  case get_sentinel_master(CurrentSentinel, MasterName) of
    {error, {sentinel_error, _}} ->
      get_master(OtherSentinels, MasterName);
    % {error, not_a_master} ->
    %   get_sentinel_master(CurrentSentinel, MasterName);
    Result ->
      Result
  end;
get_master([], _MasterName) ->
  {error, all_sentinels_down}.





get_sentinel_master({Host, Port}, MasterName) ->
    case gen_tcp:connect(Host, Port, ?SOCKET_OPS) of
        {ok, Socket} ->
            SendRes = send(Socket, ["SENTINEL", " ", "get-master-addr-by-name", " ", MasterName, "\r\n"]),
            ?LOG_INFO("[REDIS] SENTINEL MASTER_INFO ~p", [SendRes]),
            Result = 
            case SendRes of
                {ok, RawData} ->
                  case parse_master_response(RawData) of
                    {ok, Addr} ->
                      {ok, Addr};
                    Error ->
                      Error
                  end;
                Error ->
                    {error, {sentinel_error, Error}}
            end,
            gen_tcp:close(Socket),
            Result;
        {error, Error} ->
            {error, {sentinel_error, Error}}
    end.




send(Socket, Data)->
    _ = gen_tcp:send(Socket, Data),
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, RawData} ->
          {ok, RawData};
        Error ->
            {error, {sentinel_error, Error}}
    end.


parse_master_response(<<"*2\r\n", Host/binary>>) ->
    [_Size, MasterHost, _PortSize, Port, _|_] = re:split(Host, "\r\n"),
    {ok, {binary_to_list(MasterHost), binary_to_integer(Port)}};
parse_master_response(Error) ->
    {error, {sentinel_error, Error}}.

