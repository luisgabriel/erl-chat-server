-module(chat_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
    Users = dict:new(), % It maps nick to pid
    PIDs = dict:new(), % It maps pid to nick
    {ok, {Users, PIDs}}.

% handle_call is invoked in response to gen_server:call
handle_call({connect, Nick}, {From, _Ref}, {Users, PIDs}) ->
    io:format("connect ~p->~p~n", [From, Nick]),
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = Users,
            NewPIDs = PIDs,
            {nick_in_use, Nick};
        false ->
            NewUsers = dict:append(Nick, From, Users),
            NewPIDs = dict:append(From, Nick, PIDs),
            ok
    end,
    {reply, Response, {NewUsers, NewPIDs}};

handle_call(disconnect, {From, _Ref}, {Users, PIDs}) ->
    Response = case dict:is_key(From, PIDs) of
        true ->
            Nick = lists:nth(1, dict:fetch(From, PIDs)),
            NewPIDs = dict:erase(From, PIDs),
            NewUsers = dict:erase(Nick, Users),
            {ok, Nick};
        false ->
            NewUsers = Users,
            NewPIDs = PIDs,
            user_not_found
    end,
    {reply, Response, {NewUsers, NewPIDs}};

handle_call(_Message, _From, State) ->
    {reply, error, State}.


handle_cast({message, From, Msg}, {Users, PIDs}) -> 
    %% TODO broadcast
    {noreply, {Users, PIDs}}.

% We get compile warnings from gen_server unless we define these
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
