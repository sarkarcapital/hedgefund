-module(fix_client).
-export([start/3, generate_message/0, send_messages/3]).

%% Start the client
%% Usage: fix_client:start("127.0.0.1", 12345, 1000000).
start(Host, Port, MessageCount) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    io:format("Connected to FIX server ~s:~p~n", [Host, Port]),
    send_messages(Socket, MessageCount, 0).

%% Generate a sample FIX message
generate_message() ->
    fix_parser:serialize([
        {"8", "FIX.4.4"},    % BeginString
        {"35", "D"},         % MessageType (Order - Single)
        {"49", "CLIENT1"},   % SenderCompID
        {"56", "SERVER1"},   % TargetCompID
        {"34", "1"},         % MsgSeqNum
        {"11", "ORDER123"},  % ClOrdID
        {"55", "AAPL"},      % Symbol
        {"54", "1"},         % Side (Buy)
        {"60", "20240101-12:30:00.000"}, % TransactTime
        {"38", "100"},       % OrderQty
        {"40", "2"},         % OrdType (Limit)
        {"44", "150.50"}     % Price
    ]).

%% Send messages repeatedly
send_messages(_Socket, MessageCount, Sent) when Sent >= MessageCount ->
    io:format("All messages sent: ~p~n", [Sent]),
    ok;

send_messages(Socket, MessageCount, Sent) ->
    Message = generate_message(),
    ok = gen_tcp:send(Socket, Message),
    io:format("Message sent (~p): ~s~n", [Sent + 1, Message]),
    send_messages(Socket, MessageCount, Sent + 1).

