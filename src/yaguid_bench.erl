
-module(yaguid_bench).

-export([bench/0, bench/2, bench/3]).

-define(DEFAULT_WORKERS, 128).
-define(DEFAULT_ITERS, 8192).


bench() ->
    bench(?DEFAULT_WORKERS, ?DEFAULT_ITERS).

bench(Workers, Iters) ->
    bench(Workers, Iters, true).

bench(Workers, Iters, CheckDuplicates) ->
    Start = erlang:system_time(millisecond),
    {CTime, End, Result} = case test_get_id(Workers, Iters, CheckDuplicates) of
        {T, Ids} ->
            E = erlang:system_time(millisecond),
            HasDuplicates = not(lists:sort(Ids) == lists:usort(Ids)),
            {T, E, #{has_duplicates => HasDuplicates}};
        T ->
            E = erlang:system_time(millisecond),
            {T, E, #{}}
    end,
    ResultCount = (Workers * Iters),
    ClockTimeMs = End - Start,
    ClockTimeS = ClockTimeMs / 1000.0,
    CumulativeTimeMs = CTime div 1000,
    AvgTime = (CTime div ResultCount) / 1000.0,
    Result#{avg_time_ms => AvgTime,
            clock_time_ms => ClockTimeMs,
            rate_per_s => round(ResultCount / ClockTimeS),
            cumulative_time_ms => CumulativeTimeMs}.


test_get_id(Workers, Iters, false) ->
    Self = self(),
    Fun = fun() -> run_get_id(Self, Iters, 0) end,
    [spawn(Fun) || _ <- lists:seq(1, Workers)],
    collect_result(Workers, 0);
test_get_id(Workers, Iters, true) ->
    Self = self(),
    Fun = fun() -> run_get_id(Self, Iters, {0, []}) end,
    [spawn(Fun) || _ <- lists:seq(1, Workers)],
    collect_result(Workers, {0, []}).


run_get_id(Dst, 0, Acc) ->
    Dst ! {result, Acc};
run_get_id(Dst, Iters, {TimeAcc, IdsAcc}) ->
    {Time, Id} = timer:tc(fun yaguid:get_id/0),
    run_get_id(Dst, Iters-1, {TimeAcc+Time, [Id | IdsAcc]});
run_get_id(Dst, Iters, TimeAcc) ->
    {Time, _Id} = timer:tc(fun yaguid:get_id/0),
    run_get_id(Dst, Iters-1, TimeAcc+Time).


collect_result(0, {Total, Ids}) ->
    {Total, lists:append(Ids)};
collect_result(0, Total) ->
    Total;
collect_result(N, {TimeAcc, IdsAcc}) ->
    {Time, Ids} = receive
        {result, R} -> R
    after 30000 -> error(timeout)
    end,
    collect_result(N - 1, {TimeAcc + Time, [Ids | IdsAcc]});
collect_result(N, TimeAcc) ->
    Time = receive
        {result, R} -> R
    after 30000 -> error(timeout)
    end,
    collect_result(N - 1, TimeAcc + Time).
