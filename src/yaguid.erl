%%------------------------------------------------------------------------------
%% @doc yaguid - Yet Another Globally Unique Id server (snowflake-ish, in Erlang).
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid).

-include("yaguid.hrl").

%% API
-export([get_id/0,
         node_id/0,
         timestamp/0,
         node_id/1,
         local_partition_size/0,
         timestamp/1,
         seq/1]).

%% Internal API
-export([partition_ids/0, partition_ids/1]).

%%==============================================================================
%% Constants
%%==============================================================================

-define(APPLICATION, ?MODULE).

%%==============================================================================
%% Types
%%==============================================================================

-export_type([id/0, node_id/0, seq/0, timestamp/0, local_partition_size/0]).

%%==============================================================================
%% API
%%==============================================================================

-spec get_id() -> id().
%% @doc Get a globally unique ID.
get_id() ->
    yaguid_srv:get_id().


-spec node_id() -> node_id().
%% @doc Get the configured node ID.
node_id() ->
    application:get_env(?APPLICATION, node_id, 0).


-spec timestamp() -> timestamp().
%% @doc Returns an epoch timestamp in milliseconds.
timestamp() ->
    erlang:system_time(millisecond).


-spec local_partition_size() -> local_partition_size().
%% @doc Returns the number of local partitions the application should run for
%%      issuing IDs. Must be a power of 2.
local_partition_size() ->
    application:get_env(?APPLICATION, local_partition_size, 1).


-spec timestamp(id()) -> timestamp().
%% @doc Returns the epoch timestamp (in milliseconds) used in the given ID.
timestamp(Id) when is_integer(Id), Id >= 0 ->
    <<Timestamp:41, _/bitstring>> = <<Id:64>>,
    Timestamp + ?EPOCH_MS_OFFSET.


-spec node_id(id()) -> node_id().
%% @doc Returns the node id used in the given ID.
node_id(Id) when is_integer(Id), Id >= 0 ->
    <<_Timestamp:41, NodeId:10, _/bitstring>> = <<Id:64>>,
    NodeId.


-spec seq(id()) -> seq().
%% @doc Returns the node id used in the given ID.
seq(Id) when is_integer(Id), Id >= 0 ->
    <<_Timestamp:41, _NodeId:10, Seq:12, _/bitstring>> = <<Id:64>>,
    Seq.

%%==============================================================================
%% Internal API
%%==============================================================================

-spec partition_ids() -> [partition_id()].
%% @doc The set of all partition IDs. Primarily for internal use.
partition_ids() ->
    partition_ids(local_partition_size()).


-spec partition_ids(local_partition_size()) -> [partition_id()].
%% @doc The set of all partition IDs for the given local_partition_size.
%%      Primarily for internal use.
partition_ids(LocalPartitionSize) ->
    lists:seq(0, LocalPartitionSize - 1).
