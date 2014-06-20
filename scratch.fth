1 10 .. ( 1+ dup 2 * ) map


: map [] map' ;

: map'
    (( [ H T .] Fun Acc ))
    T Fun [ H Fun call Acc .] map'

    (( [] _ Acc ))
    Acc reverse
;


: drop (( X )) ;
: dup (( X )) X ;
: swap (( X Y )) Y X ;
: over (( X Y )) X Y X ;
: rot (( X Y Z )) Y Z X ;
: -rot (( X Y Z )) Z X Y ;
: nip (( X Y )) Y ;
: tuck (( X Y )) Y X Y ;

: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;
