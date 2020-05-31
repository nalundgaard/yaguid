%%------------------------------------------------------------------------------
%% @doc yaguid_srv eunit tests
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_srv_tests).

-include("yaguid.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMESTAMP, 1590915735564).

state_setup() ->
    yaguid_sup_tests:setup(1023, 2).


state_teardown(SupRes) ->
    yaguid_sup_tests:teardown(SupRes).


state_test_() ->
    {setup,
     fun state_setup/0,
     fun state_teardown/1,
     [{"get initial state", test_get_initial_state()},
      {"get single id", test_get_single_id()},
      {"get updated state", test_get_updated_state()}]}.


test_get_initial_state() ->
    [?_assertMatch(#{last_seq := undefined,
                     last_ts := _,
                     max_seq := 2047,
                     node_id := 1023,
                     partition_bits := 1,
                     partition_id := 0},
                   yaguid_srv:get_state(0)),
     ?_assertMatch(#{last_seq := undefined,
                     last_ts := _,
                     max_seq := 2047,
                     node_id := 1023,
                     partition_bits := 1,
                     partition_id := 1},
                   yaguid_srv:get_state(1)),
     ?_assertMatch([#{last_seq := undefined,
                      last_ts := _,
                      max_seq := 2047,
                      node_id := 1023,
                      partition_bits := 1,
                      partition_id := 0},
                    #{last_seq := undefined,
                      last_ts := _,
                      max_seq := 2047,
                      node_id := 1023,
                      partition_bits := 1,
                      partition_id := 1}],
                   yaguid_srv:get_states())].


test_get_single_id() ->
    [?_assertEqual(0,
                   yaguid:seq(yaguid_srv:get_id(0))),
     ?_assertEqual(2048,
                   yaguid:seq(yaguid_srv:get_id(1)))].


test_get_updated_state() ->
    [?_assertMatch(#{last_seq := 0,
                     last_ts := _,
                     max_seq := 2047,
                     node_id := 1023,
                     partition_bits := 1,
                     partition_id := 0},
                   yaguid_srv:get_state(0)),
     ?_assertMatch(#{last_seq := 0,
                     last_ts := _,
                     max_seq := 2047,
                     node_id := 1023,
                     partition_bits := 1,
                     partition_id := 1},
                   yaguid_srv:get_state(1))].


get_id_setup() ->
    yaguid_sup_tests:setup(1, 32).


get_id_teardown(SupRes) ->
    yaguid_sup_tests:teardown(SupRes).


get_id_test_() ->
    {setup,
     fun get_id_setup/0,
     fun get_id_teardown/1,
     [{"get_id no duplicates", fun test_get_id_no_duplicates/0},
      {"get_id partition ordered", fun test_get_id_partition_ordered/0}]}.


test_get_id_no_duplicates() ->
    Ids = [yaguid_srv:get_id() || _ <- lists:seq(0, 500)],
    ?assertEqual(lists:sort(Ids), lists:usort(Ids)).


test_get_id_partition_ordered() ->
    IdsByPart = [[yaguid_srv:get_id(PartitionId) || _ <- lists:seq(0, 500)]
                 || PartitionId <- yaguid:partition_ids()],
    [?assertEqual(Ids, lists:usort(Ids)) || Ids <- IdsByPart].


get_id_timestamp_range_test_() ->
    NodeId = 1023,
    OldTimestamp = ?EPOCH_MS_OFFSET - 10000,
    MaxInBoundsTimestamp = ?TIMESTAMP_MASK + ?EPOCH_MS_OFFSET, % (1 bsl 41) - 1
    OutOfBoundsTimestamp = ?TIMESTAMP_MASK + ?EPOCH_MS_OFFSET + 1, % 1 bsl 41
    [{"timestamps within 41 bits after EPOCH_MS_OFFSET adjustment are valid",
      ?_assertEqual(MaxInBoundsTimestamp,
                    yaguid:timestamp(yaguid_srv:get_id(MaxInBoundsTimestamp, NodeId, 1, 1, 0)))},
     {"timestamps greater than 41 bits after EPOCH_MS_OFFSET adjustment wrap around",
      ?_assertEqual(?EPOCH_MS_OFFSET,
                    yaguid:timestamp(yaguid_srv:get_id(OutOfBoundsTimestamp, NodeId, 1, 1, 0)))},
     {"timestamps before EPOCH_MS_OFFSET raise exceptions (0)",
      ?_assertError(timestamp_out_of_bounds,
                    yaguid:timestamp(yaguid_srv:get_id(0, NodeId, 1, 1, 0)))},
     {"timestamps before EPOCH_MS_OFFSET raise exceptions",
      ?_assertError(timestamp_out_of_bounds,
                    yaguid:timestamp(yaguid_srv:get_id(OldTimestamp, NodeId, 1, 1, 0)))}].


get_id_node_id_range_test_() ->
    TS = ?TEST_TIMESTAMP,
    PartId = 1,
    PartBits = 1,
    Seq = 0,
    [{"node id of 1024 wraps to 0",
      ?_assertEqual(0,
                    yaguid:node_id(yaguid_srv:get_id(TS, 1024, PartId, PartBits, Seq)))}].
