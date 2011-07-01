-module(client).

-import(client_server).
-export([start/0, connect/2, disconnect/0]).

% These are all wrappers for calls to the server
start() -> client_server:start().

connect(ServerPID, Nick) ->
	gen_server:call(client_server, {connect, ServerPID, Nick}).

disconnect() ->
	gen_server:call(client_server, disconnect).


