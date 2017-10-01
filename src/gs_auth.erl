-module(gs_auth).

-behaviour(cowboy_middleware).

-include("../include/gs_user.hrl").

%% cowboy_middleware callbacks
-export([execute/2]).

%==============================================
% Cowboy Middleware Callbacks
%==============================================

execute(Req, Env) ->
    case is_authorized(Req, Env) of
        {ok, SessionId, Username, LastUpdated} ->
            {ok, Req, Env#{handler_opts := #user{name = Username, sessionid = SessionId}}};
        _ ->
            io:format("~p~n", [failed_to_auth]),
            reject(Req, Env)
    end.

%==============================================
% Internal functions
%==============================================

is_authorized(Req, _Env) ->
    Cookie = cowboy_req:parse_cookies(Req),
    case get_session_id(Cookie) of
        {ok, SessionId} ->
            case gs_session:get_session(SessionId) of
                {ok, {session, Id, Username, LastUpdated}} ->
                    {ok, Id, Username, LastUpdated};
                _ ->
                    {error, no_such_user}
            end;
        {error, no_such_sessionid} ->
            {error, no_such_user}
    end.

reject(Req, _Env) ->
	{stop, cowboy_req:reply(401, #{}, <<>>, Req)}.

get_session_id(Cookie) ->
    case lists:keyfind(<<"sessionid">>, 1, Cookie) of
        {<<"sessionid">>, SessioId} ->
            {ok, SessioId};
        _ ->
            {error, no_such_sessionid}
    end.
