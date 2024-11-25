-module(app).
-export([helloworld/0, conc_hello/1]).

helloworld() ->
  io:fwrite("hello world!~n").

conc_hello(N) ->
  [spawn(fun helloworld/0) || _ <- lists:seq(1, N)].
