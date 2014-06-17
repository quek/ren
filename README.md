# ren(仮 chon) 

Forth ライクな言語

- Erlang で書かれている
- Erlang VM の BEAM 上で実行される

## 実行方法

```
% erl +pc unicode
1> c("ren").
{ok,ren}
2> ren:i().
1 2 + .
```

## Erlang 関数の呼び出し

module:function/arity で Erlang の関数を呼び出せる。

```
[ 1 2 3 ] lists:reverse/1
```
