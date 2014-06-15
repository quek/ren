( 10 fib 55 == ) assert

: test-=[A,B]
    = [ A B ]
    A B +
;

( [ 1 2 ] test-=[A,B] 3 == ) assert

" ok" .
