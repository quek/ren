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
        Else call
    ;;
        _
        Then call
    ;case
;

: assert
    = Form
    Form call
    case
        false
        " failed: ~p\n" [ Form ] format
        ;;
        _
        nop
    ;case
;

: hello
    " Hello World!"
    .
;
