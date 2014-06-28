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
: cons$ (( X Y )) [ Y X .] ;


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
        ( nip 2 ( >tuple ) map-split-at >map )
        ( cons }' )
        if
    )
    if
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

: when ( ) if ;

: and (( false _ )) false (( _ false )) false (( _ _ )) true ;
: or (( false false )) false (( _ _ )) true ;


: " [ key "' ; immediate
: "'
    (( [[ key " , ]] ))
    ] compile? ( , ) when
    (( [[ key \ , ]] ))
    key backslash-char "'
    (( ))
    key "'
;
: backslash-char
    (( [[ key a , ]] ))  # bell
    7
    (( [[ key b , ]] ))  # backspace
    8
    (( [[ key t , ]] ))  # horizontal tab
    9
    (( [[ key n , ]] ))  # new line
    10
    (( [[ key v , ]] ))  # vertical tab
    11
    (( [[ key f , ]] ))  # form feed
    12
    (( [[ key r , ]] ))  # carriage ret
    13
    (( [[ key \ , ]] ))  # back slash
    [[ key \ , ]]
    (( ))
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
: each$
    (( _ [] ))
    (( F [ H T .] ))
    H F call F T each$
;

: reverse [] swap ( cons$ ) each ;

: map (( List F )) [ List F each ] ;

: reduce swapd each ;

: take (( List N )) [ List N take' ;
: take'
    (( _ 0 )) ]
    (( [] _ )) ]
    (( [ H T .] N ))
    H T N 1- take'
;

: split-at (( List N )) [ List N split-at' ;
: split-at'
    (( T 0 )) ] T
    (( [] _ )) ] []
    (( [ H T .] N ))
    H T N 1- split-at'
;

: each-split-at
    (( _ 0 _ ))
    (( [] _ _ ))
    (( List N F ))
    List N split-at >r F call r> N F each-split-at
;

: map-split-at (( List N F )) [ List N F each-split-at ] ;


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
