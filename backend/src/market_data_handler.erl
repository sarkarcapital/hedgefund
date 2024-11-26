-module(market_data_handler).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2, handle_call/3, handle_cast/2]).

% API to start the server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

% Initialization
init(_) ->
    {ok, #{}}.

% Example of processing a market data message
handle_info({process_market_data, Data}, State) ->
    io:format("Processing market data: ~p~n", [Data]),
    % Add your business logic here
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

