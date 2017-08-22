-module(init_tables).

-export([init/0]).

-record(user, {username, password, salt}).

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