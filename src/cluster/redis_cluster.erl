-module(redis_cluster).
-include("redis.hrl").

%% API.
-export([worker/2]).
-export([get_all_pools/1]).
-export([reload_pools/1]).


worker(PoolName, Command)->
    PoolKey = get_key_from_command(Command),
    Slot = get_key_slot(PoolKey),
    {PoolNameBySlot, Version} = get_pool_by_slot(PoolName, Slot),
    {PoolNameBySlot, Version}.



get_all_pools(PoolName) ->
    Pools = redis_settings:cluster_pools_get(PoolName),
    lists:usort([Pool#cluster_pool.name || Pool <- Pools#cluster_pools.pools]).








reload_pools(State) ->
    PoolName = State#state.name,
    Slots = get_slots(State#state.cluster_nodes),

    {Pools, Nodes} = lists:foldl(fun([StartSlot, EndSlot | [[Host0, Port0 | _] | _]], {PoolsAcc, NodesAcc})->

        Host = binary_to_list(Host0),
        Port = binary_to_integer(Port0),
        % start pool
        PoolNameBySlot = get_name(PoolName, Host, Port),
        PoolRes =
        erlpool:start_pool(PoolNameBySlot, [
            {size, State#state.size},
            {group, redis_pool},
            {start_mfa, {eredis, start_link, [Host, Port]}}
        ]),
        case PoolRes of
            ok -> ok;
            {error,{already_started,_}} -> ok;
            PoolErr ->
                exit(PoolErr)
        end,

        Pool = #cluster_pool{
            s_start = binary_to_integer(StartSlot),
            s_end = binary_to_integer(EndSlot),
            name = PoolNameBySlot
        },
        {[Pool | PoolsAcc], [{Host, Port} | NodesAcc]}

    end, {[], []}, Slots),

    NewVersion = State#state.cluster_version + 1,
    PrevPools  = redis_settings:cluster_pools_get(PoolName),
    NewPools   = #cluster_pools{
        pools   = Pools,
        version = NewVersion
    },
    redis_settings:cluster_pools_set(PoolName, NewPools),
    ?LOG_INFO("[REDIS] NEW SLOTS ~p", [Pools]),

    %% close old pools
    lists:foreach(fun(#cluster_pool{name = Name})-> 
        case lists:keyfind(Name, 2, Pools) of
            false -> 
                    catch erlpool:stop_pool(Name),
                    ?LOG_INFO("[REDIS] CLOSED SLOT ~p", [Name]);
            _ -> ok
        end
    end, PrevPools#cluster_pools.pools),

    State#state{
        cluster_version = NewVersion,
        cluster_nodes   = lists:usort(Nodes) ++ State#state.init_settings
    }.







%%%%%%%%%%%%% LOCAL





get_name(PoolName, Host, Port) ->
    list_to_atom(atom_to_list(PoolName) ++ Host ++ "#" ++ integer_to_list(Port)).



get_slots([]) ->
    undefined;
get_slots([{Host, Port} | Nodes])->
    
    process_flag(trap_exit, true),
    Result = eredis:start_link(Host, Port, 0, "", no_reconnect, 500),
    process_flag(trap_exit, false),

    case Result of
        {ok, Connection} ->
          
          case eredis:q(Connection, ["CLUSTER", "SLOTS"]) of

            {ok, ClusterInfo} ->
                eredis:stop(Connection),
                ?LOG_INFO("[REDIS] SLOTS OK", []),
                ClusterInfo;

            {error,<<"ERR unknown command 'CLUSTER'">>} = Err ->
                ?LOG_ERROR("[REDIS] err ~s ~p", [Host, Err]),
                eredis:stop(Connection),
                get_slots(Nodes);

            {error,<<"ERR This instance has cluster support disabled">>} = Err ->
                ?LOG_ERROR("[REDIS] err ~s ~p", [Host, Err]),
                eredis:stop(Connection),
                get_slots(Nodes);

            ErrOther ->
                ?LOG_ERROR("[REDIS] err ~s ~p", [Host, ErrOther]),
                eredis:stop(Connection),
                get_slots(Nodes)
        end;
        _ ->
            ?LOG_ERROR("[REDIS] err ~s ~p", [Host, Result]),
            get_slots(Nodes)
  end.














get_pool_by_slot(PoolName, Slot)->
    Pools = redis_settings:cluster_pools_get(PoolName),
    PoolNameBySlot = get_pool_by_slot_item(Slot, Pools#cluster_pools.pools),
    if
        PoolNameBySlot =/= undefined ->
            {PoolNameBySlot, Pools#cluster_pools.version};
        true ->
            {undefined, Pools#cluster_pools.version}
    end.
get_pool_by_slot_item(_, [])->
    undefined;
get_pool_by_slot_item(Slot, [#cluster_pool{s_start = Start, s_end = End, name = Name} | _])when Slot >= Start, Slot =< End ->
    Name;
get_pool_by_slot_item(Slot, [_ | Rest])->
    get_pool_by_slot_item(Slot, Rest).





%% =============================================================================
%% @doc Return the first key in the command arguments.
%% In a normal query, the second term will be returned
%%
%% If it is a pipeline query we will use the second term of the first term, we
%% will assume that all keys are in the same server and the query can be
%% performed
%%
%% If the pipeline query starts with multi (transaction), we will look at the
%% second term of the second command
%%
%% For eval and evalsha command we will look at the fourth term.
%%
%% For commands that don't make sense in the context of cluster
%% return value will be undefined.
%% @end
%% =============================================================================
get_key_from_command([[X|Y]|Z]) when is_bitstring(X) ->
    get_key_from_command([[bitstring_to_list(X)|Y]|Z]);
get_key_from_command([[X|Y]|Z]) when is_list(X) ->
    case string:to_lower(X) of
        "multi" ->
            get_key_from_command(Z);
        _ ->
            get_key_from_command([X|Y])
    end;
get_key_from_command([Term1,Term2|Rest]) when is_bitstring(Term1) ->
    get_key_from_command([bitstring_to_list(Term1),Term2|Rest]);
get_key_from_command([Term1,Term2|Rest]) when is_bitstring(Term2) ->
    get_key_from_command([Term1,bitstring_to_list(Term2)|Rest]);
get_key_from_command([Term1,Term2|Rest]) ->
    case string:to_lower(Term1) of
        "info" ->
            undefined;
        "config" ->
            undefined;
        "shutdown" ->
            undefined;
        "slaveof" ->
            undefined;
        "eval" ->
            get_key_from_rest(Rest);
        "evalsha" ->
            get_key_from_rest(Rest);
        _ ->
            Term2
    end;
get_key_from_command(_) ->
    undefined.

%% =============================================================================
%% @doc Get key for command where the key is in th 4th position (eval and
%% evalsha commands)
%% @end
%% =============================================================================
get_key_from_rest([_,KeyName|_]) when is_bitstring(KeyName) ->
    bitstring_to_list(KeyName);
get_key_from_rest([_,KeyName|_]) when is_list(KeyName) ->
    KeyName;
get_key_from_rest(_) ->
    undefined.

get_key_slot(Key) when is_bitstring(Key) ->
    get_key_slot(bitstring_to_list(Key));
get_key_slot(Key) ->
    KeyToBeHased = case string:chr(Key,${) of
        0 ->
            Key;
        Start ->
            case string:chr(string:substr(Key,Start+1),$}) of
                0 ->
                    Key;
                Length ->
                    if
                        Length =:= 1 ->
                            Key;
                        true ->
                            string:substr(Key,Start+1,Length-1)
                    end
            end
    end,
    redis_cluster_hash:hash(KeyToBeHased).