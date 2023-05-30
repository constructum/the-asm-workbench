freetype PHIL       == { phil : INT }
freetype PHIL_STATE == { thinking, hungry, eating }

freetype FORK       == { fork : INT }
freetype FORK_STATE == { free, used_by : PHIL }


static function n == 5

static function Phil == { phil(i) | i in {0..n-1} }
static function PhilState == { thinking, hungry, eating }

static function Fork == { fork(i) | i in {0..n-1} }
static function ForkState == { free } union { used_by(ph) | ph in Phil }

static function left (phil(i))  == fork(i)
static function right (phil(i)) == fork((i+1) mod n)


dynamic function phil_state : PHIL -> PHIL_STATE
with phil_state (ph) in PhilState
initially MAP_TO_FUN { ph -> thinking | ph in Phil }

dynamic function fork_state : FORK -> FORK_STATE
with fork_state (fo) in ForkState
initially MAP_TO_FUN { fo -> free | fo in Fork }


external function self : PHIL
with self in Phil

external function decide_to_eat : BOOL
with decide_to_eat in { true, false }

external function decide_to_stop_eating : BOOL
with decide_to_stop_eating in { true, false }


transition GetFork (ph, fo) ==
  if fork_state (fo) = free
  then fork_state (fo) := used_by (ph)
  endif

transition GetForks (ph) ==
  if fork_state (left(ph)) = free
  then GetFork (ph, left(ph))
       if fork_state (right(ph)) = free
       then GetFork (ph, right(ph))
            phil_state(ph) := eating    // got both forks => eating
       else phil_state(ph) := hungry    // only one fork => hungry
       endif
  elseif fork_state (right(ph)) = free
  then GetFork (ph, right(ph))
       phil_state(ph) := hungry         // only one fork => hungry
  else phil_state(ph) := hungry         // no forks at all => hungry
  endif

transition ReleaseForks (ph) ==
  fork_state (left(ph))  := free
  fork_state (right(ph)) := free
  phil_state(ph) := thinking


derived relation HasBothForks (ph) ==
  fork_state (left (ph)) = used_by (ph)
  and fork_state (right(ph)) = used_by (ph)


transition Program ==
  case phil_state (self) of
    thinking :
      if decide_to_eat
      then GetForks (self)
      endif ;
    hungry :
      GetForks (self) ;
    eating :
      if decide_to_stop_eating
      then ReleaseForks (self)
      endif
  endcase
