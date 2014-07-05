[ 1 2 ] = [ X Y ]
[ 1 2 ] (( [ X Y ] )) =

( X Y + 3 == ) assert
# call_block を変えないとだめ



: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;



#" fib 5 is #{5 fib} and fib 10 is #{10 fib}."




: length type-of dispatch length ;

:m length erlang:length/1 ;

:m length
(( Tupul tuple )) length
;

:m length
(( X tao )) X 'body-length @
;


:g foo type-of ;
:m foo drop 'default ;
:m foo (( list )) 'l ;
:m foo (( tuple )) 't ;
:m foo (( list )) 'l!!! ;

1 foo
[] foo
{ 1 } foo

