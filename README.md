# ren(仮 chon)

a concatenative language.

Erlang で書いた Forth や Factor に似た言語。


## 実行方法

```
% erl +pc unicode
1> c("ren").
{ok,ren}
2> ren:i().
1 2 + .
```


## データスタックに対するパターマッチ

```
: reverse [] swap reverse' ;
: reverse'
    (( [] ))
    (( Acc [ H T .] )) [ H Acc .] T reverse'
;
( [ 1 2 3 ] reverse [ 3 2 1 ] == ) assert

: map [ -rot map' ;
: map'
    (( [] _  )) ]
    (( [ H T .] F ))
    H F call T F map'
;
( [ 1 2 3 ] ( 2 * ) map [ 2 4 6 ] == ) assert
```


## Erlang 関数の呼び出し

module:function/arity で Erlang の関数を呼び出せる。

```
[ 1 2 3 ] lists:reverse/1
```

関数を引数にとるものは >fun/1 などで Erlang の関数に変換する。

```
( 2 * ) >fun/1 [ 1 2 3 ]  lists:map/2
```
