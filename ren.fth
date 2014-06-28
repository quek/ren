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
# これはコメント

: postpone
    ' ,
    ; immediate
: #! postpone # ; immediate
#! これもコメント

: [']
    'lit , 'lit , ' ,
; immediate





# Shuffle words
: drop (( _ )) ;
: dup (( X )) X X ;
: swap (( X Y )) Y X ;
: over (( X Y )) X Y X ;
: rot (( X Y Z )) Y Z X ;
: -rot (( X Y Z )) Z X Y ;
: nip (( _ X )) X ;
: tuck (( X Y )) Y X Y ;

: swapd (( X Y Z )) Y X Z ;


: [] [ ] ;

: cons (( X Y )) [ X Y .] ;

: >tuple erlang:list_to_tuple/1 ;
: >map maps:from_list/1 ;

: { '{ ;
: #{ '#{ ;

: } [] }' ;
: }'
    over '{ ==
    ( nip >tuple )
    (
        over '#{ ==
        ( nip [] >map' )
        ( cons }' )
        if
    )
    if
;
: >map'
    (( [ K V T .] Acc ))
    T [ { K V } Acc .] >map'
    (( [] Acc ))
    Acc >map
;

: at # map key -- value
    swap maps:get/2
;


: 1+ 1 + ;
: 1- 1 - ;

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


: each
    (( [] _ ))
    (( [ H T .] F ))
    H F call T F each
;

: reverse [] swap ( swap cons ) each ;

: map [ -rot each ] ;

: reduce swapd each ;



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
    ( '" . )
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
