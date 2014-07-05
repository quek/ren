[ 1 2 ] = [ X Y ]

( X Y + 3 == ) assert
# call_block を変えないとだめ




: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;





#" fib 5 is #{5 fib} and fib 10 is #{10 fib}."



# { } マップで {{ }} タプル にするか

{ " a" 1 " b" 2 " c" 3 }
{ ' a 1 ' b 2 ' c 3 }

{ a: 1 b: 2 c: 3 } a: at

{ :a 1 :b 2 :c 3 } :a at

{ 'a 1 'b 2 'c 3 } 'a at

[ 'a 1 'b 2 'c 3 ] 2 split_every ( >tuple ) map


: length generic ;

:g length
(( Tupul tuple )) length
;

:g length
(( X tao )) X 'body-length @
;