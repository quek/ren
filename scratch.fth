test ( then ) ( else ) if
[ 1 10 .. ] ( 1+ dup 2 * ) map

( 1 1+ 2 == ) assert


: test_case
    case
        [ ]
        ( 11111
          22222
          " empty" )
        [ H _ .]
        ( H )
    ;case
    .
;
