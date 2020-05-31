[![Build Status](https://travis-ci.com/nalundgaard/yaguid.svg?token=3pga7Eqxpyy3pGz7Ty7C&branch=master)](https://travis-ci.com/nalundgaard/yaguid)

# yaguid

Yet Another Globally Unique ID server ([snowflake-ish][snowflake], in Erlang).

Like [snowflake][snowflake], it generates 64-bit integer IDs that are roughly chronologically
ordered by the inclusion of the timestamp in the ID.

## ID structure

`yaguid` IDs are constructed from the following data:

| ID segment   | value                          | bits |
|-------------:|--------------------------------|-----:|
|  _timestamp_ | Epoch timestamp (milliseconds) |   41 |
|    _node_id_ | Fixed ID (0-1023)              |   10 |
|   _sequence_ | Ordering sequence (0-4095)     |   12 |
|     _unused_ | 0                              |    1 |
|    __total__ |                                |   64 |

The `timestamp` is derived from the Unix epoch timestamp (in milliseconds), with an offset
(subtracted) of the timestamp for January 1, 2020 12:00:00 AM (GMT), maximizing the time before the
timestamp exhausts the 41 bits allocated in the ID, which, based on this offset, will be
September 6, 2089 10:47:35 (GMT). Note that if your system time is set prior to
January 1, 2020 12:00:00 AM (GMT), `yaguid` will not issue IDs.

The `node_id` is fixed in the application (see [configuration](#configure)), and allows independent
instances of the application to issue IDs concurrently without any chance of ID collision.

The `sequence` number is added after the `timestamp` and `node_id`. This value is incremented
sequentially within its 12 bits of space (0-4095) when subsequent IDs are issued within the same
`timestamp`. When the `timestamp` increments, the sequence is always reset to 0. If the sequence
space is exhausted within a millisecond, `yaguid` will pause issuing new IDs until the `timestamp`
increments.

The final bit in the ID is unused, as per the [snowflake][snowflake] format.

## Architecture



## Build

`yaguid` requires Erlang/OTP 20+. It is built using [rebar3][rebar3], but a working binary is
included.

To build, ensure that you have an appropriate Erlang/OTP installed in your `$PATH`, and run:

```bash
$ make
```

## Configure

The node can be configured by editing the `shell.config` file in the [config][cfg] directory. By
default, when starting the node, this file will be sourced from the
[shell.config.tmpl][shell_cfg_tmpl] file in the same directory. You can edit this file as desired:

```erlang
[
    {yaguid, [
        {node_id, 0},
        {local_partition_size, 16}
    ]}
].
```

|                parameter | type              | default |
|-------------------------:|-------------------|--------:|
|                _node_id_ | integer (0..1023) |       0 |
| _local_partition_size_\* | integer (2..64)   |      16 |

\*`local_partition_size` is the number of independent ID-issuing local `gen_server`s that
will independently issue. It must be a power of 2

Note that these parameters cannot be dynamically changed; the `yaguid` application must be restarted
in order for changes to take effect.

## Run

```bash
$ make shell
```

## Test

```bash
$ make eunit
```

## Benchmark

A basic benchmarking tool is included in the application, `yaguid_bench`. This can be executed from
an Erlang shell:

```bash
$ make shell
```

```erlang
> yaguid_bench:bench().
> Workers = 256, Iters = 4096, CheckDuplicates = true.
> yaguid_bench:bench(Workers, Iters).
yaguid_bench:bench(Workers, Iters, CheckDuplicates).
```

|        __parameter__ | __meaning__                                                       |
|---------------------:|-------------------------------------------------------------------|
|        _avg_time_ms_ | average time (in milliseconds) to call `get_id/0`                 |
|    _clock_time_ms_\* | time (in milliseconds) to execute benchmark                       |
| _cumulative_time_ms_ | cumulative time (in ms) spent getting IDs by all workers          |
| _has_duplicates\*\*_ | (boolean) whether any generated IDs contained a duplicate         |
|         _rate_per_s_ | total number of IDs generated divided by cumulative time (in sec) |

\* `clock_time_ms` includes the time it takes to marshal results, so it may be biased towards making
the performance (including `rate_per_s`) appear slower, particularly with `CheckDuplicates = true`
set (default).

\*\* `has_duplicates` is only included if `CheckDuplicates = true` is passed (default).

Example local interaction:

```erlang
1> io:format("~36p~n", [yaguid_bench:bench(128, 1024, true)]).
#{avg_time_ms => 0.1,
  clock_time_ms => 111,
  cumulative_time_ms => 13185,
  has_duplicates => false,
  rate_per_s => 1180829}
ok
2> io:format("~36p~n", [yaguid_bench:bench(256, 2048, true)]).
#{avg_time_ms => 0.176,
  clock_time_ms => 406,
  cumulative_time_ms => 92699,
  has_duplicates => false,
  rate_per_s => 1291350}
ok
3> io:format("~36p~n", [yaguid_bench:bench(512, 4096, true)]).
#{avg_time_ms => 0.352,
  clock_time_ms => 1577,
  cumulative_time_ms => 739914,
  has_duplicates => false,
  rate_per_s => 1329836}
ok
```


[snowflake]: https://github.com/twitter-archive/snowflake/blob/snowflake-2010/README.mkd
[rebar3]: https://www.rebar3.org 
[cfg]: config
[shell_cfg_tmpl]: config/shell.config.tmpl
