-module(benchmark).
-export([helloworld/0, conc_hello/0]).

helloworld() ->
    io:fwrite("hello world!~n").

%% spawn 10,000 instances of helloworld() and measure the time
conc_hello() ->
    StartTime = erlang:monotonic_time(),
    Pids = [spawn(fun helloworld/0) || _ <- lists:seq(1, 10000)],
    %% Wait for processes to finish (this can be optimized based on the use case)
    lists:foreach(fun(Pid) -> erlang:exit(Pid, normal) end, Pids),
    %%EndTime = erlang:monotonic_time(),
    Duration = erlang:monotonic_time() - StartTime,
    io:format("Total time for completion: ~p~n", [Duration]).