module: core

: >map                                    # [ k1 v1 k2 v2 ...] -- map
    2 ( >tuple ) map-split-at maps:from_list/1 ;

: #{ '>map '{ ;

: #{} #{ } ;

: at # map key -- value
    swap maps:get/2
;

: at! (( Map Key Value ))
    Key Value Map maps:put/3
;

: key? (( Map Key )) Key Map maps:is_key/2 ;

: >typed-map (( List Type ))
    List >map '-type- Type at!
;
