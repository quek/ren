: lf
    lit [[ key

    , ]]
;

: # key #' ; immediate
: #'
    (( [[ lf , ]] ))
    (( _ ))
    key #'
;
: #! postpone # ; immediate

# Shuffle words
: drop (( _ )) ;
: dup (( X )) X X ;
: swap (( X Y )) Y X ;
: over (( X Y )) X Y X ;
: rot (( X Y Z )) Y Z X ;
: -rot (( X Y Z )) Z X Y ;
: nip (( _ X )) X ;
: tuck (( X Y )) Y X Y ;


: [] [ ] ;

: cons (( X Y )) [ X Y .] ;


: 1+
    1 +
;

: 1-
    1 -
;

: 0? (( 0 )) true (( _ )) false ;

: ! (( false )) true (( _ )) false ;

: if (( Then Else ))
    case
        false
        ( Else call )
        _
        ( Then call )
    ;case
;

: assert (( Form ))
    Form call
    case
        false
        ( " failed: ~p\n" [ Form ] format )
        _
        ( )
    ;case
;


: reverse [] swap reverse' ;
: reverse'
    (( [] ))
    (( Acc [ H T .] )) [ H Acc .] T reverse'
;

: map [ -rot map' ; # list function -- [ list function
: map'
    (( [] _  )) ]
    (( [ H T .] F ))
    H F call T F map'
;

: reduce
    (( [] Acc _ )) Acc
    (( [ H T .] Acc F ))
    T H Acc F call F reduce
;

: .. [ -rot ..' ;
: ..'
    (( X X ))
    X ]
    (( X Y ))
    X X 1+ Y ..'
;


: ."
    compile?
    ( postpone " ['] . , )
    ( ' " . )
    if
; immediate



# #############################################################################
# おもちゃ
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
    ." Hello World!"
;
