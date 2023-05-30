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


transition GetFork (ph, fo) ==
  if fork_state (fo) = free
  then fork_state (fo) := used_by (ph)
  endif

transition GetForks (ph) ==
  GetFork (ph, left(ph))
  GetFork (ph, right(ph))

transition ReleaseForks (ph) ==
  block
    fork_state (left(ph))  := free
    fork_state (right(ph)) := free
  endblock

derived relation HasBothForks (ph) ==
  fork_state (left (ph)) = used_by (ph)
  and fork_state (right(ph)) = used_by (ph)


transition Program ==
  case phil_state (self) of
    thinking :
      phil_state(self) := hungry
      GetForks (self) ;
    hungry :
      if HasBothForks (self)
      then phil_state(self) := eating
      else GetForks (self)
      endif ;
    eating :
      ReleaseForks (self)
      phil_state(self) := thinking
  endcase
