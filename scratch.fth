1 10 .. ( 1+ dup 2 * ) map


: map [] map' ;

: map'
    (( [ H T .] Fun Acc ))
    T Fun [ H Fun call Acc .] map'

    (( [] _ Acc ))
    Acc reverse
;
