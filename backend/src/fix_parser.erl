-module(fix_parser).
-export([parse/1, serialize/1]).

% Parse a FIX message string into a list of {Key, Value} tuples.
parse(Msg) ->
    MsgList = string:tokens(Msg, "|"),
    lists:map(fun parse_pair/1, MsgList).

parse_pair(Pair) ->
    case string:split(Pair, "=") of
        [Key, Value] -> {Key, Value};
        _ -> {error, invalid_format}
    end.

% Serialize a list of {Key, Value} tuples into a FIX message string.
serialize(KeyValuePairs) ->
    lists:foldl(fun({K, V}, Acc) -> Acc ++ K ++ "=" ++ V ++ "|" end, "", KeyValuePairs).

