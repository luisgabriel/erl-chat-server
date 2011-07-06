-module(chat_server).

-export([start/1, pre_loop/1]).

start(Port) ->
    controller:start(),
    tcp_server:start(?MODULE, Port, {?MODULE, pre_loop}).

pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            io:format("Nick: ~p~n", [Nick]),
            case Command of
                "CONNECT" ->
                    try_connection(clean(Nick), Socket);
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(controller, {connect, Nick, Socket}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "CONNECT:OK:" ++ List ++ "\n"),
            gen_server:cast(controller, {join, Nick}),
            loop(Nick, Socket);
        nick_in_use ->
            gen_tcp:send(Socket, "CONNECT:ERROR:Nick in use.\n"),
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
                    say(Nick, Socket, clean(Content));
                "PVT" ->
                    {ReceiverNick, [_|Msg]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Content),
                    private_message(Nick, Socket, ReceiverNick, clean(Msg));
                "QUIT" ->
                    quit(Nick, Socket)
            end;
        {error, closed} ->
            ok
    end.

say(Nick, Socket, Content) ->
    gen_server:cast(controller, {say, Nick, Content}),
    loop(Nick, Socket).

private_message(Nick, Socket, ReceiverNick, Msg) ->
     gen_server:cast(controller, {private_message, Nick, ReceiverNick, Msg}),
     loop(Nick, Socket).

quit(Nick, Socket) ->
    Response = gen_server:call(controller, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye.\n"),
            gen_server:cast(controller, {left, Nick}),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors.\n"),
            ok
    end.

clean(Data) ->
    string:strip(Data, both, $\n).
