-module(user_default).

-export([to_list/1,
         banner/0,
         prompt/1]).

banner() ->
    Command = io_lib:format("figlet -t -c ~p", [node()]),
    Output = os:cmd(Command),
    io:format(Output).

prompt(_Previous) ->
    "λ ".

-spec to_list(term()) -> [term()].
to_list(Tuple) when is_tuple(Tuple) ->
    lists:map(fun to_list/1, tuple_to_list(Tuple));
to_list(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_list(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_list(Map) when is_map(Map) ->
    lists:map(fun to_list/1, maps:to_list(Map));
to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_list(Other) ->
    Other.
