%%------------------------------------------------------------------------------
%% @doc yaguid partitioned ID-issuing server.
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(yaguid_srv).

-behaviour(gen_server).

-include("yaguid.hrl").

%% API
-export([start_link/3,
         get_id/0, get_id/1,
         get_state/1, get_states/0]).

%% Internal API
-export([partition_srv_name/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%==============================================================================
%% Constants
%%==============================================================================

-define(SERVER, ?MODULE).

%%==============================================================================
%% Types
%%==============================================================================

-record(state, {last_ts :: undefined | yaguid:timestamp(),
                last_seq :: undefined | seq(),
                max_seq = ?SEQ_MASK :: seq(),
                node_id = 0 :: node_id(),
                partition_id = 0 :: partition_id(),
                partition_bits = 0 :: non_neg_integer()}).

-type state() :: #state{}.

%%==============================================================================
%% API
%%==============================================================================

-spec start_link(node_id(), partition_id(), partition_bits()) ->
    {ok, pid()} |
    {error, {already_started, pid()}} |
    {error, Reason :: term()}.
start_link(NodeId, PartitionId, PartitionBits) when NodeId =< ?NODE_ID_MASK,
                                                    PartitionId < ?MAX_LOCAL_PARTITIONS,
                                                    PartitionBits =< ?MAX_PARTITION_BITS ->
    InitArgs = [NodeId, PartitionId, PartitionBits],
    gen_server:start_link({local, partition_srv_name(PartitionId)}, ?MODULE, InitArgs, []);
start_link(NodeId, PartitionId, PartitionBits) ->
    erlang:error(badarg, [NodeId, PartitionId, PartitionBits]).


-spec get_id() -> id().
%% @doc Generate a random partition ID and get an ID from it.
get_id() ->
    gen_server:call(random_partition_srv_pid(), get_id).


-spec get_id(partition_id()) -> id().
%% @doc Given a partition ID, get an ID from it.
get_id(PartitionId) ->
    gen_server:call(partition_srv_pid(PartitionId), get_id).


-spec get_state(partition_id()) -> maps:map().
%% @doc Given a partition ID, get its state (in map format).
get_state(PartitionId) ->
    gen_server:call(partition_srv_pid(PartitionId), get_state).


-spec get_states() -> [maps:map()].
%% @doc Get the state maps for all the partitions.
get_states() ->
    [get_state(PartitionId)
     || PartitionId <- yaguid:partition_ids()].

%%==============================================================================
%% Internal API
%%==============================================================================

-spec partition_srv_name(partition_id()) -> atom().
%% @doc Generate the partition gen_server name (atom) for the given partition
%%      ID. For internal use.
partition_srv_name(PartitionId) ->
    binary_to_atom(partition_srv_name_bin(PartitionId), latin1).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

init([NodeId, PartitionId, PartitionBits]) ->
    timer:sleep(1), %% sleep for 1 ms to ensure that this process cannot reissue
                    %% an ID from a previously crashed server with the same
                    %% partition ID
    MaxSeq = ?SEQ_MASK bsr PartitionBits,
    {ok, #state{node_id = NodeId,
                last_ts = yaguid:timestamp(),
                partition_id = PartitionId,
                partition_bits = PartitionBits,
                max_seq = MaxSeq}}.


handle_call(get_id, _From, State = #state{node_id = NodeId,
                                          partition_id = PartitionId,
                                          partition_bits = PartitionBits}) ->
    {NextSeq, NextTS, NextState} = next_seq_and_ts(State),
    Id = get_id(NextTS, NodeId, PartitionId, PartitionBits, NextSeq),
    {reply, Id, NextState};
handle_call(get_state, _From, State) ->
    {reply, state_to_map(State), State};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec partition_srv_name_bin(partition_id()) -> binary().
%% @private
partition_srv_name_bin(PartitionId) ->
    <<"yaguid_partition_srv_", (integer_to_binary(PartitionId))/binary>>.


-spec random_partition_srv_pid() -> undefined | pid().
%% @private
random_partition_srv_pid() ->
    random_partition_srv_pid(yaguid:local_partition_size()).


-spec random_partition_srv_pid(local_partition_size()) -> undefined | pid().
%% @private
random_partition_srv_pid(1 = _LocalPartitionSize) ->
    partition_srv_pid(0);
random_partition_srv_pid(LocalPartitionSize) ->
    PartitionId = rand:uniform(LocalPartitionSize) - 1,
    partition_srv_pid(PartitionId).


-spec partition_srv_pid(partition_id()) -> pid().
%% @private
partition_srv_pid(PartitionId) ->
    Label = binary_to_existing_atom(partition_srv_name_bin(PartitionId), latin1),
    whereis(Label).


-spec next_seq_and_ts(state()) ->
    {ok, {NextSeq :: seq(), NextTS :: yaguid:timestamp(), NewState :: state()}}.
%% @private
next_seq_and_ts(#state{last_seq = undefined} = State) ->
    Seq = 0,
    TS = yaguid:timestamp(),
    {Seq, TS, State#state{last_seq = Seq, last_ts = TS}};
next_seq_and_ts(#state{last_ts = LastTS, last_seq = LastSeq, max_seq = MaxSeq} = State) ->
    case yaguid:timestamp() of
        LastTS when LastSeq >= MaxSeq ->
            % out of sequence numbers to issue ID, wait till timestamp rollover
            timer:sleep(1),
            next_seq_and_ts(State);
        LastTS ->
            Seq = LastSeq + 1,
            {Seq, LastTS, State#state{last_seq = Seq}};
        NewTs ->
            Seq = 0,
            {Seq, NewTs, State#state{last_ts = NewTs, last_seq = Seq}}
    end.


-spec get_id(timestamp(), node_id(), partition_id(), partition_bits(), seq()) -> id().
%% @private
get_id(Timestamp0, NodeId, PartitionId, PartitionBits, Seq) when Timestamp0 >= ?EPOCH_MS_OFFSET->
    Timestamp1 = Timestamp0 - ?EPOCH_MS_OFFSET,
    Timestamp = Timestamp1 band ?TIMESTAMP_MASK, %% mask timestamp to 41 bits.
    SeqBits = ?SEQ_BITS - PartitionBits,
    <<Id:64>> = <<Timestamp:41/unsigned-integer-big-unit:1,
                  NodeId:10/unsigned-integer-big-unit:1,
                  PartitionId:PartitionBits/unsigned-integer-big-unit:1,
                  Seq:SeqBits/unsigned-integer-big-unit:1,
                  0:1>>,
    Id;
get_id(Timestamp, NodeId, PartitionId, PartitionBits, Seq) ->
    erlang:error(timestamp_out_of_bounds, [Timestamp, NodeId, PartitionId, PartitionBits, Seq]).


-spec state_to_map(state()) -> maps:map().
%% @private
state_to_map(State) ->
    maps:from_list(lists:zip(record_info(fields, state),
                             tl(tuple_to_list(State)))).
