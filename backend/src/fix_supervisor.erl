-module(fix_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    % Define child processes
    Children = [
        {fix_connection_handler,
         {connection_handler, start_link, [12345]}, % Port 12345
         permanent, 5000, worker, [connection_handler]},
        {market_data_handler,
         {market_data_handler, start_link, []},
         permanent, 5000, worker, [market_data_handler]}
    ],
    % Strategy
    {ok, {{one_for_one, 5, 10}, Children}}.

