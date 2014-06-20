: drop (( _ )) ;
: dup (( X )) X X ;
: swap (( X Y )) Y X ;
: over (( X Y )) X Y X ;
: rot (( X Y Z )) Y Z X ;
: -rot (( X Y Z )) Z X Y ;
: nip (( _ X )) X ;
: tuck (( X Y )) Y X Y ;

: [] [ ] ;

: 1+
    1 +
;

: 1-
    1 -
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

: reverse [] reverse' ;
: reverse'
    (( [] Acc )) Acc
    (( [ H T .] Acc )) T [ H Acc .] reverse'
;

: map [] map' ;
: map'
    (( [] _ Acc ))
    Acc reverse
    (( [ H T .] F Acc ))
    T F [ H F call Acc .] map'
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
