-module(app).
-export([helloworld/0, conc_hello/0]).

helloworld() ->
  io:fwrite("hello world!~n").

%% spawn 10'000 instances of helloworld()
conc_hello() ->
  [spawn(fun helloworld/0) || _ <- lists:seq(1, 10000)].
