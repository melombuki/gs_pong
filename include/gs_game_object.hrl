-define(PADDLE1, "paddle1").
-define(PADDLE2, "paddle2").
-define(BALL, "ball").
-define(ROOT, "root").

-record(game_object, {name, x, y, width, height, color, render, children}).