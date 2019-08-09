-module(user_default).

-include_lib("xmerl/include/xmerl.hrl").

-export([to_list/1,
         banner/0,
         prompt/1,

         fmap/2,
         bind/2,
         '=<<'/2,
         '>>='/2
        ]).

-type result(A) :: {ok, A} | {error, term()}.

-export_type([result/1]).

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

-spec fmap(fun ((A) -> B), result(A)) -> result(B).
fmap(F, {ok, V}) ->
    {ok, F(V)};
fmap(_F, Error={error, _}) ->
    Error.

-spec bind(fun ((A) -> result(B)), result(A)) -> result(B).
bind(F, {ok, V}) ->
    F(V);
bind(_F, Error={error, _}) ->
    Error.

-spec '=<<'(fun ((A) -> result(B)), result(A)) -> result(B).
'=<<'(F, {ok, V}) ->
    F(V);
'=<<'(_F, Error={error, _}) ->
    Error.

-spec '>>='(result(A), fun ((A) -> result(B))) -> result(B).
'>>='({ok, V}, F) ->
   F(V);
'>>='(Error={error, _}, _F) ->
    Error.
