-module(init_tables).

-export([init/0,
         init_session_table/0,
         migrate/0]).

-record(user, {username, password, salt}).
-record(session, {id, user_name, last_updated}).

init() ->
    spawn(fun() -> observer:start() end),
    mnesia:create_schema([node()]),
    mnesia:start(),
    crypto:start(),
    bcrypt:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]),
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Pass} = bcrypt:hashpw(<<"123">>, Salt),
    mnesia:dirty_write({user, <<"melom">>, Pass, Salt}).

init_session_table() ->
    mnesia:create_table(session, [{attributes, record_info(fields, session)}, {disc_copies, [node()]}]).

migrate() ->
    F = fun({session, Id, Username}, New) -> #session{id = Id, user_name = Username} end,
    mnesia:transform_table(session, F, record_info(fields, session), session).