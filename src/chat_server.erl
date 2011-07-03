-module(chat_server).

-export([start/1, pre_loop/1]).

start(Port) ->
    controller:start(),
    tcp_server:start(?MODULE, Port, {?MODULE, pre_loop}).

pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary_to_list(Data),
            {Command, [_|Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Command of
                "CONNECT" ->
                    try_connection(Nick, Socket);
                _ ->
                    gen_tcp:send(Socket, "Unknown command!"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(controller, {connect, Nick, Socket}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "CONNECT:ok:" ++ List),
            loop(Nick, Socket);
        nick_in_use ->
            gen_tcp:send(Socket, "CONNECT:error:Nick in use."),
            ok
    end.

loop(Nick, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Command of
                "SAY" ->
                    say(Nick, Socket, Content);
                "PVT" ->
                    private_message(Nick, Socket);
                "QUIT" ->
                    quit(Nick, Socket)
            end;
        {error, closed} ->
            ok
    end.

say(Nick, Socket, Content) ->
    gen_server:cast(controller, {say, Nick, Content}),
    loop(Nick, Socket).

private_message(Nick, Socket) ->
    ok.

quit(Nick, Socket) ->
    io:format("Nick to remove: ~p~n", [Nick]),
    Response = gen_server:call(controller, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye."),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors."),
            ok
    end.
