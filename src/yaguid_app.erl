%%------------------------------------------------------------------------------
%% @doc yaguid_app - top-level application behaviour.
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% application callbacks
%%==============================================================================

start(_StartType, _StartArgs) ->
    NodeId = yaguid:node_id(),
    LocalPartitionSize = yaguid:local_partition_size(),
    yaguid_sup:start_link(NodeId, LocalPartitionSize).


stop(_State) ->
    ok.
