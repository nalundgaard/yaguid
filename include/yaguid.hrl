%%------------------------------------------------------------------------------
%% @doc yaguid.hrl - constants for yaquid application
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-ifndef(yaguid_hrl).
-define(yaguid_hrl, 1).


-define(SUPERVISOR, yaguid_sup).
-define(PARTITION_SUPERVISOR, yaguid_partition_sup).

-type id() :: non_neg_integer(). %% 64 bit integer ID

-type node_id() :: 0..1023.

-type seq() :: 0..4095.

%% epoch timestamp (in milliseconds)
-type timestamp() :: non_neg_integer().

%% local_partition_size
%%  The number of independent local ID-issuing processes to run.
%%  Power of 2 =< 64. Its square root is the number of bits in the sequence
%%  fragment allocated to identifying each process. The theoretical max is
%%  12 bits (4096 processes), but seems unwise (too much parallelism, not enough
%%  capacity to increment the state within a millisecond).
-type local_partition_size() :: 1..64.

%% partition_id
%%  An individual partition number for an ID-issuing process, 0-indexed.
-type partition_id() :: 0..63.

%% partition_bits
%%  The number of bits to use in the partition part of the sequence number. The
%%  actual value is computed at run-time from the local_partition_size.
-type partition_bits() :: 0..6.

%% EPOCH_MS_OFFSET
%% This value is Friday, January 1, 2020 12:00:00 AM (GMT)
-define(EPOCH_MS_OFFSET, 1577836800000).

-define(MAX_LOCAL_PARTITIONS, 64).
-define(MAX_PARTITION_BITS, 6).
-define(SEQ_BITS, 12).
-define(SEQ_MASK, 4095). % 12 bits, 1 bsl 12 - 1
-define(NODE_ID_BITS, 10).
-define(NODE_ID_MASK, 1023). % 10 bits, 1 bsl 10 - 1
-define(TIMESTAMP_MASK, 2199023255551). % 41 bits, 1 bsl 41 - 1

-endif.
