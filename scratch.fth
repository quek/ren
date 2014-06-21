1 10 .. ( 1+ dup 2 * ) map



[ 1 2 ] = [ X Y ]

( X Y + 3 == ) assert






: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;
