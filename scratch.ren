[ 1 2 ] = [ X Y ]
[ 1 2 ] (( [ X Y ] )) =
1 2 (( X Y )) =

( X Y + 3 == ) assert
# call_block を変えないとだめ



: bi (( X P Q )) X P call X Q call ;
: tri (( X P Q R )) X P call X Q call X R call ;



#" fib 5 is #{5 fib} and fib 10 is #{10 fib}."



# TODO map 引数のパターンマッチ

: point{ ( 'point >typed-map ) '{ ;
point{ 'x X1 X2 + 'y Y1 Y2 + }

:g core.type-of dup biw.type-of ;
:m type-of nip ;
:m type-of (( #{ -type- X } map )) X ;

:g + over type-of over type-of ;     # x y -- x y type-of-x type-of-y
:m + (( list list )) erlang:'++'/2 ;
:m + (( #{ x X1 y Y1 } #{ x X2 y Y2 } point point ))
    piont{ 'x X1 X2 + 'y Y1 Y2 + }
;

" ab" " cd" +  # => " abcd"
point{ 'x 1 'y 2 } point{ 'x 3 'y 1 } +  # => point{ 'x 4 'y 3 }




:m + (( integer X ))     number X + ;
:m + (( float   X ))     number X + ;
:m + (( X integer ))     X number + ;
:m + (( X   float ))     X number + ;
:m + (( number number )) erlang:+/2 ;

# 実際のところ、これでいい。
:m + (( integer X ))     erlang:+/2 ;
:m + (( float   X ))     erlang:+/2 ;

