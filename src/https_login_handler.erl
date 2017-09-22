-module(https_login_handler).

-export([init/2,
         terminate/3]).

-import(mochijson2, [decode/1]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            handle_post(Req, State) ;
        _ ->
            reject(Req, State, 405)
    end.

handle_post(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    {struct, JsonData} = decode(Body),
    Type     = proplists:get_value(<<"type">>, JsonData),
    Username = proplists:get_value(<<"username">>, JsonData),
    Password = proplists:get_value(<<"password">>, JsonData),
    case Type of
        <<"login">> ->
            handle_login(Username, Password, Req, State);
        <<"signup">> ->
            handle_signup(Username, Password, Req, State);
        _ ->
            reject(Req, State, 400)
    end.

handle_login(Username, Password, Req, State) ->
    UserRecord = mnesia:dirty_read({user, Username}),
    case UserRecord of
        [{user, Username, Hash, Salt}] ->
            {ok, Hash} = bcrypt:hashpw(base64:decode(Password), Salt),
            accept(Username, Req, State);
        _ ->
            reject(Req, State, 401)
    end.

handle_signup(Username, Password, Req, State) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Pass} = bcrypt:hashpw(Password, Salt),
    mnesia:dirty_write({user, Username, Pass, Salt}),
    accept(Username, Req, State).

accept(Username, Req, State) ->
    SessionId = base64:encode(crypto:strong_rand_bytes(32)),
    Object = riakc_obj:new(<<"session">>, SessionId, Username),
    gs_riak_client:put(Object),
    Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId, Req, #{secure => true}),
    Req3 = cowboy_req:reply(200, Req2),
    {ok, Req3, State}.

reject(Req, State, Code) ->
    Rec2 = cowboy_req:reply(Code, Req),
    {ok, Rec2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
    