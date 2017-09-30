-module(gs_session_service).

%% API
-export([get_session/1,
         create_session/2,
         invalidate_session/1,
         invalidate_all_sessions/0,
         refresh_session/1]).

-record(session, {id, user_name, last_updated}).
-define(TABLE, session).

get_session(SessionId) ->
    case mnesia:dirty_read({?TABLE, SessionId}) of
        [Session] ->
            {ok, Session};
        _ ->
            {error, no_such_session}
    end.

refresh_session(SessionId) ->
    case get_session(SessionId) of
        {ok, Session} ->
            ok = mnesia:dirty_write(?TABLE, Session#session{last_updated = os:system_time(millisecond)}),
            ok;
        Other ->
            Other
    end.

create_session(SessionId, Username) ->
    ok = mnesia:dirty_write(?TABLE, #session{id = SessionId, user_name = Username, last_updated = os:system_time(millisecond)}),
    ok.

invalidate_session(SessionId) ->
    ok = mnesia:dirty_delete(?TABLE, SessionId),
    ok.

invalidate_all_sessions() ->
    {atomic, ok} = mnesia:clear_table(?TABLE),
    ok.