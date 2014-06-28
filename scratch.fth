[ 1 2 ] = [ X Y ]

( X Y + 3 == ) assert
# call_block を変えないとだめ






: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;


in: foo.bar

use: aaa.bbb.ccc as: c
use: nnn.mmm

ff
c.ff
xxx.yyy.ff


#" fib 5 is #{5 fib} and fib 10 is #{10 fib}."



# { } マップで {{ }} タプル にするか

{ " a" 1 " b" 2 " c" 3 }
{ ' a 1 ' b 2 ' c 3 }

{ a: 1 b: 2 c: 3 } a: at

{ :a 1 :b 2 :c 3 } :a at

{ 'a 1 'b 2 'c 3 } 'a at


