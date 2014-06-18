( 10 fib 55 == ) assert

: test-=[A,B]
    = [ A B ]
    A B +
;
( [ 1 2 ] test-=[A,B] 3 == ) assert

( [ 1 2 3 ] lists:reverse/1 [ 3 2 1 ] == ) assert

: my-erlang-erverse
    lists:reverse/1
;
( [ 3 2 1 ] my-erlang-erverse [ 1 2 3 ] == ) assert


( ( 2 * ) >fun/1 [ 1 2 3 ]  lists:map/2 [ 2 4 6 ] == ) assert

( ( + ) >fun/2 0 [ 1 2 3 ] lists:foldl/3 6 == ) assert

" ok" .
