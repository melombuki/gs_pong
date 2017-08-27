-module(https_login_handler).

-export([init/2,
         terminate/3]).

init(Req, State) ->
    io:format("~p~n~p~n", ["Trying to login", Req]),
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            io:format("Made it to the body"),
            Body = <<"<h1>This is a response for POST</h1>">>,
            Req2 = cowboy_req:reply(200, #{}, Body, Req),
            {ok, Req2, State};
        _ ->
            Rec2 = cowboy_req:reply(405, #{}, <<>>, Req),
            {ok, Rec2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
	