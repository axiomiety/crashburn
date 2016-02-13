-module(drop_us).
-export([fall_velocity/2]).

fall_velocity(_Planemo, Distance) -> math:sqrt(2*9.8*Distance).
