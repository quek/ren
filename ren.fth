'core module

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


: module: ' module ; immediate                  # module: foo
: use-module: ' dup use-module-as ; immediate   # use-module: foo
: use-module-as: ' ' use-module-as ; immediate  # use-module-as: foo f




# #############################################################################
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


# #############################################################################
: use-module dup use-module-as ;


# #############################################################################
# if, etc

: true 'true ;
: false 'false ;

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


# #############################################################################
# List
: [ '[ ;

: ] [] ]' ;

: ]'
    over '[ ==
    ( nip )
    ( cons ]' )
    if
;

: .] ]' ;

: car (( [] )) [] (( [ X _ .] )) X ;
: cdr (( [] )) [] (( [ _ X .] )) X ;


: >tuple erlang:list_to_tuple/1 ;

: { '>tuple '{ ;

: } [] }' ;
: }'
    over '{ ==
    ( nip swap call )
    ( cons }' )
    if
;


" map.fth" load


: 1+ 1 + ;
: 1- 1 - ;

: 0? (( 0 )) true (( _ )) false ;

: ! (( false )) true (( _ )) false ;



: " [ key "' ; immediate
: "'
    (( [[ key " , ]] ))
    ] compile? ( , ) when
    (( [[ key \ , ]] ))
    key backslash-char-code "'
    (( ))
    key "'
;
: backslash-char-code
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
        ( " failed: ~p\n" [ Form ] format dump-context . )
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


: cons$ (( X Y )) [ Y X .] ;

: reverse (( List )) [] List ( cons$ ) each ;

: map (( List F )) [ List F each ] ;

: reduce (( List Init F )) Init List F each ;

: length 0 ( drop 1+ ) reduce ;

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
module: scratch

: fib (( N ))
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
