-module(init_tables).

-export([init/0]).

-record(user, {username, password}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]),
    mnesia:stop().