-module(controller).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
    Users = dict:new(), % It maps nick to socket
    {ok, Users}.

% handle_call is invoked in response to gen_server:call
handle_call({connect, Nick, Socket}, _From, Users) ->
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = Users,
            nick_in_use;
        false ->
            NewUsers = dict:append(Nick, Socket, Users),
            {ok, user_list(NewUsers)}
    end,
    {reply, Response, NewUsers};

handle_call({disconnect, Nick}, _From, Users) ->
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = dict:erase(Nick, Users),
            ok;
        false ->
            NewUsers = Users,
            user_not_found
    end,
    {reply, Response, NewUsers};

handle_call(_Message, _From, State) ->
    {reply, error, State}.


% handle_cast is invoked in response to gen_server:cast
handle_cast({say, Nick, Msg}, Users) ->
    broadcast(Nick, "SAID:" ++ Nick ++ ":" ++ Msg ++ "\n", Users),
    {noreply, Users};

handle_cast({private_message, Nick, Receiver, Msg}, Users) ->
    Temp = dict:find(Receiver, Users),
    case Temp of
        {ok, [Socket|_]} ->
            gen_tcp:send(Socket, "PVT:" ++ Nick ++ ":" ++ Msg ++ "\n");
        _ ->
            ok
    end,
    {noreply, Users};

handle_cast({join, Nick}, Users) ->
    broadcast(Nick, "JOIN:" ++ Nick ++ "\n", Users),
    {noreply, Users};

handle_cast({left, Nick}, Users) ->
    broadcast(Nick, "LEFT:" ++ Nick ++ "\n", Users),
    {noreply, Users};

handle_cast(_Message, State) ->
    {noreply, State}.


% auxiliary functions
broadcast(Nick, Msg, Users) ->
    Sockets = lists:map(fun({_, [Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, ":").

% Definitions to avoid gen_server compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
