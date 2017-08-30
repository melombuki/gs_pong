-module(gs_auth).
-behaviour(cowboy_middleware).

-include("../include/gs_user.hrl").

-export([execute/2]).

execute(Req, Env) ->
    case is_authorized(Req, Env) of
        {ok, Username} ->
            {ok, Req, Env#{handler_opts := #user{name = Username}}};
        _ ->
            io:format("~p~n", [failed_to_auth]),
            reject(Req, Env)
    end.

%==============================================
% Internal functions
%==============================================

is_authorized(Req, _Env) ->
    Cookie = cowboy_req:parse_cookies(Req),
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, SessionId} = get_session_id(Cookie),
	case Resp = riakc_pb_socket:get(Pid, <<"session">>, SessionId) of
		{ok, {riakc_obj, <<"session">>, _, _, [{_, Username}], _, _} } -> % holy F, is this necessary?
            {ok, Username};
		_ ->
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
