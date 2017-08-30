-module(https_login_handler).

-export([init/2,
         terminate/3]).

-import(mochijson2, [decode/1]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            {ok, Body, _} = cowboy_req:read_body(Req),
            {struct, JsonData} = decode(Body),
            Username = proplists:get_value(<<"username">>, JsonData),
            Password = proplists:get_value(<<"password">>, JsonData),
            case User = mnesia:dirty_read({user, Username}) of
                [{user, Username, Hash, Salt}] ->
                    {ok, NewHash} = bcrypt:hashpw(base64:decode(Password), Salt),
                    {Hash = NewHash, Username},
                    SessionId = base64:encode(crypto:strong_rand_bytes(32)),
                    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
                    Object = riakc_obj:new(<<"session">>, SessionId, Username),
                    riakc_pb_socket:put(Pid, Object),
                    Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId, Req, #{secure => true}),
                    Req3 = cowboy_req:reply(200, Req2),
                    {ok, Req3, State};
                _ ->
                    reject(Req, State, 401)
            end;
        _ ->
            reject(Req, State, 405)
    end.

reject(Req, State, Code) ->
    Rec2 = cowboy_req:reply(Code, Req),
    {ok, Rec2, State}.

terminate(_Reason, _Req, _State) ->
    ok.