[ 1 10 .. ] ( 1+ dup 2 * ) map


: map [] map' ;

: map'
    s( [ H T .] Fun Acc )
    T Fun [ H Fun call Acc .] map'

    s( [] _ Acc )
    Acc reverse
;

[ 1 2 3 ] lists:reverse/1
\ re:run("abc:bar/0", "\\A(.+):(.+)/(.+)\\z", [{capture, [1, 2, 3], list}]).