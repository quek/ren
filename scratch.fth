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