-module(drop_case).
-export([fall_velocityA/2, fall_velocityB/2]).

fall_velocityA(Planemo, Distance) when Distance >= 0
  -> case Planemo of
      earth -> math:sqrt(2*9.8*Distance);
      moon -> math:sqrt(2*1.6*Distance);
      mars -> math:sqrt(2*3.71*Distance)
  end.

fall_velocityB(Planemo, Distance) when Distance >= 0
  -> Gravity = case Planemo of
      earth -> 9.8;
      moon -> 1.6;
      mars -> 3.71
  end,
  math:sqrt(2*Gravity*Distance).
