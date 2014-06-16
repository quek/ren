: 1+
    1 +
;

: 1-
    1 -
;

: swap
    = A
    = B
    A B
;

: drop
    = _
;

: if
    = Else
    = Then
    case
        false
        ( Else call )
        _
        ( Then call )
    ;case
;

: assert
    = Form
    Form call
    case
        false
        ( " failed: ~p\n" [ Form ] format )
        _
        ( nop )
    ;case
;

: fib
    = N
    N 2 =<
    case
        true
        ( 1 )
        _
        ( N 1- fib N 2 - fib + )
    ;case
;

: hello
    " Hello World!"
    .
;
