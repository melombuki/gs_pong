-module(gs_auth).
-behaviour(cowboy_middleware).

-include("../include/gs_user.hrl").

-export([execute/2]).

execute(Req, Env) ->
    case is_authorized(Req, Env) of
        {true, Username} ->
            {ok, Req, Env};
        _ ->
            io:format("~p~n", [failed_to_auth]),
            reject(Req, Env)
    end.

is_authorized(Req, _Env) ->
    Qs = cowboy_req:parse_cookies(Req),
    {<<"username">>, Username} = lists:keyfind(<<"username">>, 1, Qs),
    {<<"password">>, Password} = lists:keyfind(<<"password">>, 1, Qs),
	case User = mnesia:dirty_read({user, Username}) of
		[{user, <<"melom">>, Pass}] ->
            {Pass =:= Password, Username};
		_ ->
			false
	end.

reject(Req, _Env) ->
	{stop, cowboy_req:reply(401, #{}, <<>>, Req)}.