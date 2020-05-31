%%------------------------------------------------------------------------------
%% @doc yaguid top level supervisor.
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_sup).

-behaviour(supervisor).

-include("yaguid.hrl").

%% API
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%==============================================================================
%% API
%%==============================================================================

start_link(NodeId, LocalPartitionSize) ->
    Args = [NodeId, LocalPartitionSize],
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, Args).

%%==============================================================================
%% supervisor callbacks
%%==============================================================================

init([NodeId, LocalPartitionSize]) ->
    {ok, PartitionBits} = validate_local_partitions(LocalPartitionSize),
    PartitionIds = yaguid:partition_ids(LocalPartitionSize),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [child_spec(NodeId, PartitionId, PartitionBits) ||
                  PartitionId <- PartitionIds],
    {ok, {SupFlags, ChildSpecs}}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec child_spec(node_id(), partition_id(), partition_bits()) ->
    supervisor:child_spec().
%% @private Supervisor child specification for a worker process.
child_spec(NodeId, PartitionId, PartitionBits) ->
    #{id => yaguid_srv:partition_srv_name(PartitionId),
      start => {yaguid_srv, start_link, [NodeId, PartitionId, PartitionBits]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.


-spec validate_local_partitions(local_partition_size()) ->
    {ok, partition_bits()}.
%% @private Validate that the configured local_partitions parameter is a power
%%          of 2, and return its bit size.
validate_local_partitions(N) when is_integer(N), N > 0, N =< ?MAX_LOCAL_PARTITIONS ->
    SqrtFloor = floor(math:log2(N)),
    case (N bsr SqrtFloor) bsl SqrtFloor of
        N -> {ok, SqrtFloor};
        _ -> erlang:error(local_partitions_not_power_of_2, [N])
    end;
validate_local_partitions(N) ->
    erlang:error(local_partitions_not_power_of_2, [N]).
