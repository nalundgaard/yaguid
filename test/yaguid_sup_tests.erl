%%------------------------------------------------------------------------------
%% @doc yaguid_sup eunit tests
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_sup_tests).

-include_lib("eunit/include/eunit.hrl").

setup(NodeId, LocalPartitionSize) ->
    application:set_env(yaguid, node_id, NodeId),
    application:set_env(yaguid, local_partition_size, LocalPartitionSize),
    {ok, Pid} = yaguid_sup:start_link(NodeId, LocalPartitionSize),
    erlang:unlink(Pid),
    {yaguid_sup, Pid}.


teardown({yaguid_sup, SupPid}) ->
    application:unset_env(yaguid, local_partition_size),
    application:unset_env(yaguid, node_id),
    true = erlang:exit(SupPid, normal),
    timer:sleep(1). %% make sure the supervisor children exit


validate_local_partitions_test_() ->
    [{"2 partitions is valid and 1 bit wide",
      ?_assertEqual({ok, 1},
                    yaguid_sup:validate_local_partitions(2))},
     {"4 partitions is valid and 2 bits wide",
      ?_assertEqual({ok, 2},
                    yaguid_sup:validate_local_partitions(4))},
     {"64 partitions is valid and 6 bits wide",
      ?_assertEqual({ok, 6},
                    yaguid_sup:validate_local_partitions(64))},
     {"0 partitions is invalid",
      ?_assertError(local_partitions_not_power_of_2,
                    yaguid_sup:validate_local_partitions(0))},
     {"63 partitions is invalid",
      ?_assertError(local_partitions_not_power_of_2,
                    yaguid_sup:validate_local_partitions(63))},
     {"128 partitions is invalid (more than max)",
      ?_assertError(local_partitions_not_power_of_2,
                    yaguid_sup:validate_local_partitions(128))}].
