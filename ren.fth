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
        11111
        22222
        " empty"
        ;;
        [ H _ .]
        H
    ;case
    .
;
