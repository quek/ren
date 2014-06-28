( 10 fib 55 == ) assert

( 0 0? ) assert
( 1 0? ! ) assert

: test-=[A,B]
    = [ A B ]
    A B +
;
( [ 1 2 ] test-=[A,B] 3 == ) assert


( true ( 1 ) ( -1 ) if 1 == ) assert
( false ( 1 ) ( -1 ) if -1 == ) assert


( [ 1 2 3 ] lists:reverse/1 [ 3 2 1 ] == ) assert

: my-erlang-erverse
    lists:reverse/1
;
( [ 3 2 1 ] my-erlang-erverse [ 1 2 3 ] == ) assert


( ( 2 * ) >fun/1 [ 1 2 3 ]  lists:map/2 [ 2 4 6 ] == ) assert

( ( + ) >fun/2 0 [ 1 2 3 ] lists:foldl/3 6 == ) assert

( math:pi/0 3.141592653589793 == ) assert


: test-receive
    receive
        { a X Pid } ( Pid X send test-receive )
        { b X Pid } ( Pid X X * send )
    ;receive
;
: test-send
    'test-receive spawn
    dup
    { 'a 3 self } send
    { 'b 3 self } send
    {
        receive
          X ( X )
        ;receive
        receive
          Y ( Y )
        ;receive
    }
;
( test-send { 3 9 } == ) assert


( [ 1 2 3 ] reverse [ 3 2 1 ] == ) assert

( [ 1 2 3 ] ( 2 * ) map [ 2 4 6 ] == ) assert

( 1 10 .. 0 ( + ) reduce 55 == ) assert


# #############################################################################
# postpone ['] '
: "-and-dup
    compile?
    ( postpone " ['] dup , )
    ( '" dup )
    if
; immediate

"-and-dup b"
( cons [ " b" " b" .] == ) assert
: tset-"-and-dup
    "-and-dup a"
;
tset-"-and-dup
( cons [ " a" " a" .] == ) assert


" ok" .
