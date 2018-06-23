gs
=====

An OTP game server application.

The chat feature is loosely modeled after this repo: https://github.com/rjmholt/erlang-chat/tree/master/chat_server/src

The server makes heavy use of websockets for communication with the client. This includes the chat functionality and also handling user input events and game updates to and from the Erlang server.

This also include the use of webcomponents for the frontend which is worth checking out on its own.

Build
-----

    $ rebar3 compile
