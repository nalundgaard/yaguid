%%------------------------------------------------------------------------------
%% @doc yaguid eunit tests
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMESTAMP, 1590915735564).

get_id_setup() ->
    yaguid_sup_tests:setup(0, 64).


get_id_teardown(SupRes) ->
    yaguid_sup_tests:teardown(SupRes).


get_id_test_() ->
    {setup,
     fun get_id_setup/0,
     fun get_id_teardown/1,
     [{"get_id no duplicates", fun test_get_id_no_duplicates/0}]}.


test_get_id_no_duplicates() ->
    Ids = [yaguid:get_id() || _ <- lists:seq(0, 500)],
    ?assertEqual(lists:sort(Ids), lists:usort(Ids)).


id_props_setup() ->
    meck:new(yaguid, [passthrough]),
    meck:expect(yaguid, timestamp, 0, ?TEST_TIMESTAMP),
    {[yaguid], yaguid_sup_tests:setup(512, 1)}.


id_props_teardown({Mods, SupRes}) ->
    meck:unload(Mods),
    yaguid_sup_tests:teardown(SupRes).


id_props_test_() ->
    {setup,
     fun id_props_setup/0,
     fun id_props_teardown/1,
     [{"test timestamp is in id", fun test_timestamp_node_id_in_id/0}]}.


test_timestamp_node_id_in_id() ->
    Ids = [yaguid:get_id() || _ <- lists:seq(0, 100)],
    [?assertEqual(?TEST_TIMESTAMP, yaguid:timestamp(Id)) || Id <- Ids] ++
    [?assertEqual(512, yaguid:node_id(Id)) || Id <- Ids].
