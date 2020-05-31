%%------------------------------------------------------------------------------
%% @doc yaguid_bench eunit tests
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_bench_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    setup(0, 16).


setup(NodeId, LocalPartitionSize) ->
    application:set_env(yaguid, node_id, NodeId),
    application:set_env(yaguid, local_partition_size, LocalPartitionSize),
    ok = application:start(yaguid).


teardown(_) ->
    application:unset_env(yaguid, local_partition_size),
    application:unset_env(yaguid, node_id),
    application:stop(yaguid).


bench_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"bench test", {timeout, 30, fun test_bench/0}}]}.


test_bench() ->
    Result = yaguid_bench:bench(128, 1024),
    [?assertMatch(#{cumulative_time_ms := _,
                    has_duplicates := false},
                  Result),
     ?assert(maps:get(avg_time_ms, Result) < 1),
     ?assert(maps:get(clock_time_ms, Result) < 10000),
     ?assert(maps:get(rate_per_s, Result) > 100000)].
