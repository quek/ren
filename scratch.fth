\ defmodule Hello do
\   IO.puts "Defining the function world"
\
\   def world do
\     IO.puts "Hello World"
\   end
\
\   IO.puts "Function world defined"
\ end
\
\ Hello.world

:module Hello
  "Defining the function world" IO.puts

  : world
    "Hello World" IO.puts
  .

  "Function world defined" IO.puts
module.

Hello.world

:noname 1 + ;

:p match [ Fun Args # elixir_scope { context Context = match_vars MatchVars = } S = ]
.
:p match [ Fun Args S ]
  Args S Fun .

:p swap [ A B ] B A .
:p dup [ X ] X X .
:p drop [ X ] .


case
    [ H _ .] ->
    H
    dup
    ;;
    _ ->
    false
;case

: swap
    A =
    B =
    B A
;