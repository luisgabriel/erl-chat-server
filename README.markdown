## What is this?
A simple chat server written in Erlang.
It was built with the propose of learn a bit about the language.
## How can I run?
Compile: `erl -make`

Run Erlang VM: `erl -pa ebin/`

Run chat server: `chat_server:start(<PORT>).`
## How can I interact with the server?
It was defined a communication protocol over TCP. So you can interact with the server sending via TCP known messages.
## Protocol
The protocol messages will be described below.
### Client to Server
`CONNECT:<nick>`: Try connection with the server.

`SAY:<msg>`: Send a public message to the chat room.

`PVT:<nick>:<msg>`: Send a private message to a specific user.

`QUIT:`: Disconnect from the server.
### Server to Client
`CONNECT:OK:<nick1>:<nick2>:(...):<nickN>`: Positive response from a CONNECT message sent by client. The online user list is sent in message.

`CONNECT:ERROR:<reason>`: Negative response from a CONNECT message sent by the client.

`PVT:<nick>:<msg>`: A private message sent to the client by <nick>.

`JOIN:<nick>`: Notify when someone joined the chat room.

`LEFT:<nick>`: Notify when someone left the chat room.
## References
Here is some helpful references used during the development:

[Learn You Some Erlang for Great Good!](http://learnyousomeerlang.com/)

[A Generic Server Tutorial](http://20bits.com/articles/erlang-a-generic-server-tutorial/)

[A Generalized TCP Server](http://20bits.com/articles/erlang-a-generalized-tcp-server/)

[Erlang Examples: Talk with Sockets](http://www.zorched.net/2008/05/29/erlang-examples-talk-with-sockets/)
## Contributors
Luís Gabriel Lima ([luisgabriel](https://github.com/luisgabriel))

Rafael Brandão ([rafaelbrandao](https://github.com/rafaelbrandao))
