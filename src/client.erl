-module(client).

-import(client_server).
-export([start/0, connect/3, disconnect/0]).

start() ->
    client_server:start(),
    ServerNode = clean(io:get_line('Enter server node: ')),
    ServerPID = clean(io:get_line('Enter server Pid: ')),
    Nick = clean(io:get_line('Enter your nickname: ')),
    Response = connect(list_to_atom(ServerNode), list_to_atom(ServerPID), Nick),
    case Response of
        ok ->
            io:format('Connected to ~p~n', [ServerPID]),
            get_input();
        _ ->
            io:format('Error: ~p~n', [Response]),
            ok
    end.

get_input() ->
    Input = clean(io:get_line('Talk: ')),
    case Input of
        "disconnect" ->
            io:format('Bye.'),
            disconnect();
        _ ->
            io:format('Input: ~p~n', [Input]),
            get_input()
    end.

connect(ServerNode, ServerPID, Nick) ->
    case net_adm:ping(ServerNode) of
        pong ->
            global:sync(),
            GlobalNames = global:registered_names(),
            io:format("DBG: ~p~n",[GlobalNames]),
            case lists:any(fun(T) -> T =:= ServerPID end, GlobalNames) of
                true ->
                    gen_server:call(client_server, {connect, ServerPID, Nick});
                false ->
                    pid_not_found
            end;
        _ ->
            node_not_found
   end.

disconnect() ->
    gen_server:call(client_server, disconnect).

clean(Data) ->
    string:strip(Data, both, $\n).
