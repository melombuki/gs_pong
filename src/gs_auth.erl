-module(gs_auth).
-behaviour(cowboy_middleware).

-include("../include/gs_user.hrl").

-export([execute/2]).

execute(Req, Env) ->
    case is_authorized(Req, Env) of
        {true, Username} ->
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
    Username = get_username(Cookie),
    Password = get_password(Cookie),
	case User = mnesia:dirty_read({user, Username}) of
		[{user, <<"melom">>, Pass}] ->
            {Pass =:= Password, Username};
		_ ->
			false
	end.

reject(Req, _Env) ->
	{stop, cowboy_req:reply(401, #{}, <<>>, Req)}.
    
get_username(Cookie) ->
    case lists:keyfind(<<"username">>, 1, Cookie) of
        {<<"username">>, Name} ->
            Name;
        _ ->
            undefined
    end.

get_password(Cookie) ->
    case lists:keyfind(<<"password">>, 1, Cookie) of
        {<<"password">>, Password} ->
            Password;
        _ ->
            undefined
    end.
