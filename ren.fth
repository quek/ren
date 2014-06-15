: hello
    " Hello World!"
    .
;

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

: test_case
    case
        [ ]
        " empty" .
        ;;
        [ H _ .]
        H .
    ;case
;
