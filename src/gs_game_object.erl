-module(gs_game_object).

% API
-export([new/1,
         get_name/1,
         get_x/1,
         get_y/1,
         get_width/1,
         get_height/1,
         get_color/1,
         render/1,
         children/1,
         translate/3,
         translate/4,
         position/3,
         position/4,
         get_game_object/2,
         get_game_tree/1,
         to_proplist/1]).

-import(mochijson2, [encode/1, decode/1]).

-include("../include/gs_game_object.hrl").

%%==================================================
%% API
%%==================================================

new({Name, X, Y, Width, Height, Color, Render, Children}) ->
    {ok, #game_object{name = Name,
                      x = X,
                      y = Y,
                      width = Width,
                      height = Height,
                      color = Color,
                      render = Render,
                      children = Children}};
new(_Args) ->
    {error, bad_args}.

get_name(GameObject) ->
    GameObject#game_object.name.

get_x(GameObject) ->
    GameObject#game_object.x.

get_y(GameObject) ->
    GameObject#game_object.y.

get_width(GameObject) ->
    GameObject#game_object.width.

get_height(GameObject) ->
    GameObject#game_object.height.

get_color(GameObject) ->
    GameObject#game_object.color.

render(GameObject) ->
    GameObject#game_object.render.

children(GameObject) ->
    GameObject#game_object.children.

translate(X, Y, GameObject) ->
    UpdatedX  = GameObject#game_object.x + X,
    UpdatedY = GameObject#game_object.y + Y,
    GameObject#game_object{x = UpdatedX,
                           y = UpdatedY}.

translate(Table, GameObject, X, Y) ->
    ets:insert(Table, GameObject#game_object{x = GameObject#game_object.x + X, y = GameObject#game_object.y + Y}).
    
position(X, Y, GameObject) ->
    GameObject#game_object{x = X, y = Y}.

position(Table, GameObject, X, Y) ->
    ets:insert(Table, GameObject#game_object{x = X, y = Y}).

get_game_object(Table, Name) ->
    [GameObject] = ets:lookup(Table, Name),
    GameObject.

get_game_tree(Table) ->
    Paddle1 = get_game_object(Table, ?PADDLE1),
    Paddle2 = get_game_object(Table, ?PADDLE2),
    Ball    = get_game_object(Table, ?BALL),
    Root    = get_game_object(Table, ?ROOT),
    Root#game_object{children = [Paddle1, Paddle2, Ball]}.

%%==================================================
%% Internal Functions
%%==================================================

to_proplist(#game_object{name = Name, 
                         x = X, 
                         y = Y, 
                         width = Width, 
                         height = Height, 
                         color = Color, 
                         render = Render, 
                         children = []}) ->
    {struct, [{<<"name">>, list_to_binary(Name)},
              {<<"x">>, X},
              {<<"y">>, Y},
              {<<"width">>, Width},
              {<<"height">>, Height},
              {<<"color">>, list_to_binary(Color)},
              {<<"render">>, Render},
              {<<"children">>, []}]};
to_proplist(#game_object{name = Name, 
                         x = X, 
                         y = Y, 
                         width = Width, 
                         height = Height, 
                         color = Color, 
                         render = Render, 
                         children = Children}) ->
    JsonChildren = lists:map(fun to_proplist/1, Children),
    {struct, [{<<"name">>, list_to_binary(Name)},
              {<<"x">>, X},
              {<<"y">>, Y},
              {<<"width">>, Width},
              {<<"height">>, Height},
              {<<"color">>, list_to_binary(Color)},
              {<<"render">>, Render},
              {<<"children">>, JsonChildren}]}.
