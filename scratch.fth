1 10 .. ( 1+ dup 2 * ) map


: map [] map' ;

: map'
    (( [ H T .] Fun Acc ))
    T Fun [ H Fun call Acc .] map'

    (( [] _ Acc ))
    Acc reverse
;


( foo bar ) spawn
' baz spawn

pid msg send

receive
    pattern1
    ( block1 )
    pattern2
    ( block2 )
after 6000
    ( block )
;receive









: test-receive
    receive
        { a X Pid } ( Pid X send )
    ;receive
;

: test-send
    ' test-receive spawn
    { ' a 777 self } send
    receive
        X ( X )
    ;receive
;
