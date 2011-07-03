-module(client_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, null}.

handle_call({connect, ServerPID, Nick}, {_From, _Ref}, null) ->
    Response = gen_server:call({global, ServerPID}, {connect, Nick}),
    %io:format("~p ~n", [Response]),
    {reply, Response, ServerPID};

handle_call(disconnect, {_From, _Ref}, ServerPID) when ServerPID =/= null ->
    Response = gen_server:call({global, ServerPID}, disconnect),
    {reply, Response, null};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Library) -> {noreply, Library}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
