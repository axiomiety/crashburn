-module(drop_encap).
-export([fall_velocity/1]).

% expose a 1 arg fn and keep the 2 arg one private
fall_velocity({Planemo, Distance}) -> fall_velocity(Planemo, Distance).

fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2*9.8*Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2*1.6*Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2*3.71*Distance).
