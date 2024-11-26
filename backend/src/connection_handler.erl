-module(connection_handler).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2, terminate/2, handle_call/3, handle_cast/2]).

% API to start the connection handler
start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).

% Initialization
init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    spawn(fun() -> accept_connections(ListenSocket) end),
    {ok, ListenSocket}.

% Accept connections in a loop
accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_tcp:controlling_process(Socket, self()),
    io:format("Client connected: ~p~n", [Socket]),
    accept_connections(ListenSocket).

% Handle incoming messages
handle_info({tcp, Socket, Data}, ListenSocket) ->
    ParsedMsg = fix_parser:parse(binary:to_list(Data)),
    io:format("Received FIX message: ~p~n", [ParsedMsg]),
    Response = handle_message(ParsedMsg),
    gen_tcp:send(Socket, fix_parser:serialize(Response)),
    {noreply, ListenSocket};

handle_info({tcp_closed, Socket}, ListenSocket) ->
    io:format("Client disconnected: ~p~n", [Socket]),
    {noreply, ListenSocket};

handle_info({tcp_error, Socket, Reason}, ListenSocket) ->
    io:format("Error on socket ~p: ~p~n", [Socket, Reason]),
    {noreply, ListenSocket}.

% Handle FIX message processing
handle_message(Msg) ->
    % Example response: Echo back with an acknowledgment
    Msg ++ [{"35", "A"}, {"58", "Message received"}].

terminate(_Reason, ListenSocket) ->
    gen_tcp:close(ListenSocket),
    ok.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

