-module(ren).
-compile(export_all).
-include("ren.hrl").

immed(';', _) ->
    true;
immed('"', _) ->
    true;
immed(_, _) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

':'(#context{}=C) ->
    {Word, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[Word|S]}),
    C2#context{compile=true}.

';'(#context{here=H, latest=W}=C) ->
    Clauses = lists:foldr(fun compile_code/2,
                          {var, 1, 'C'},
                          H),
    {M, F} = module_function(W),
    Codes = [{attribute, 0, module, M},
             {attribute, 0, export, [{F, 1}, {immed, 2}]},
             {function, 0, immed, 2,
              [{clause, 0, [{atom, 0, F}, {var, 0, '_'}], [], [{atom, 0, false}]}]},
             {function, 0, F, 1, [{clause, 0, [{var, 0, 'C'}], [],
                                   [Clauses]}]}],
    io:format("~p\n", [Codes]),
    {ok, CModule, CBin} = compile:forms(Codes),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false}.

'\''(#context{s=S}=C) ->
    {Word, C2} = word(C),
    C2#context{s=[list_to_atom(Word)|S]}.

call(#context{s=[Word|S]}=C) ->
    {M, F} = find(Word, C),
    apply(M, F, [C#context{s=S}]).

'+'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B+A|S]}.

'-'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B-A|S]}.

'*'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B*A|S]}.

'/'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B/A|S]}.

'.'(#context{s=[H|T]}=C) ->
    io:format("~p\n", [H]),
    C#context{s=T}.

'['(#context{s=S}=C) ->
    C#context{s=['['|S]}.

']'(#context{s=S}=C) ->
    C#context{s=']'(S, [])}.
']'(['['|T], List) ->
    [List|T];
']'([H|T], List) ->
    ']'(T, [H|List]).

car(#context{s=[[]|T]}=C)->
    C#context{s=[[]|T]};
car(#context{s=[[X|_]|T]}=C) ->
    C#context{s=[X|T]}.

cdr(#context{s=[[]|T]}=C) ->
    C#context{s=[[]|T]};
cdr(#context{s=[[_|XS]|T]}=C) ->
    C#context{s=[XS|T]}.

'{'(#context{s=S}=C) ->
    C#context{s=['{'|S]}.

'}'(#context{s=S}=C) ->
    C#context{s='}'(S, [])}.
'}'(['{'|T], List) ->
    [list_to_tuple(List)|T];
'}'([A|D], List) ->
    '}'(D, [A|List]).

'"'(C) ->
    {X, C2} = key(C),
    '"'(C2, X, []).
'"'(#context{s=S, compile=Compile}=C, $", Acc) ->                %" %
    Str = lists:reverse(Acc),
    case Compile of
        true ->
            comma(Str, C);
        false ->
            C#context{s=[Str|S]}
    end;
'"'(C, X, Acc) ->
    {X2,  C2} = key(C),
    '"'(C2, X2, [X|Acc]).

load(C) ->
    push_source(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_function(Word) when is_atom(Word) ->
    module_function(atom_to_list(Word));
module_function(Word) ->
    {list_to_atom("forth_" ++ Word),
     list_to_atom(Word)}.

find(Word, _) ->
    {M, F} = module_function(Word),
    case erlang:function_exported(M, F, 1) of
        true ->
            {M, F};
        _ ->
            case erlang:function_exported(ren, F, 1) of
                true ->
                    {ren, F};
                _ ->
                    error
            end
    end.

push_source(#context{s=[File|S], r=R, source=Src, buffer=B}=C) ->
    {ok, In} = file:open(File, read),
    C#context{s=S, r=[Src,B|R], source=In, buffer=""}.

pop_source(#context{source=Src, r=[S,B|T]}=C) ->
    file:close(Src),
    C#context{source=S, buffer=B, r=T}.

refill(#context{source=Src}=C) ->
    case io:get_line(Src, "") of
        eof ->
            pop_source(C);
        Line ->
            C#context{buffer=Line}
    end.

key(#context{buffer=[X|XS]}=C) ->
    {X, C#context{buffer=XS}};
key(C) ->
    key(refill(C)).

word(C) ->
    case key(C) of
        {X, C2} when X < 33 ->
            word(C2);
        {X, C2} ->
            word([X], C2)
    end.

word(Word, C) ->
    case key(C) of
        {X, C2} when X < 33 ->
            {lists:reverse(Word), C2};
        {X, C2} ->
            word([X|Word], C2)
    end.

to_number(X) ->
    case string:to_integer(X) of
        {N, []} -> {ok, N};
        _ ->
            case string:to_float(X) of
                {N, []} -> {ok, N};
                _ -> error
            end
    end.

comma(Literal, #context{here=H}=C) ->
    C#context{here=[{lit, Literal}|H]}.

comma(Module, Function, #context{here=H}=C) ->
    C#context{here=[{Module, Function}|H]}.

lit(Literal, #context{s=S}=C) ->
    C#context{s=[Literal|S]}.

dup(#context{s=[H|T]}=C) ->
    C#context{s=[H,H|T]}.

bye(_) ->
    exit("bye").

literal_type(X) when is_integer(X) ->
    integer;
literal_type(X) when is_float(X) ->
    float;
literal_type(X) when is_atom(X) ->
    atom;
literal_type(X) ->
    case io_lib:char_list(X) of
        true ->
            string;
        _ ->
            unknow
    end.

compile_code({lit, Literal}, Acc) ->
    {call, 1,
     {remote, 1, {atom, 1, ren}, {atom, 1, lit}},
     [{literal_type(Literal), 1, Literal},
      Acc]};
compile_code({Module, Function}, Acc) ->
    {call, 1,
     {remote, 1, {atom, 1, Module}, {atom, 1, Function}},
     [Acc]}.

header(#context{s=[Word|S]}=C) ->
    C#context{s=S,
              latest=Word,
              here=[]}.

interpret(#context{s=S, r=R, compile=Compile, here=H}=C) ->
    io:format("next: s=~w r=~w h=~w\n", [S, R, H]),
    {Word, C2} = word(C),
    case find(Word, C) of
        error ->
            case to_number(Word) of
                {ok, N} ->
                    case Compile of
                        false ->
                            interpret(C2#context{s=[N|S]});
                        true ->
                            interpret(comma(N, C2))
                    end;
                _ ->
                    io:format("~s is unknow.\n", [Word]),
                    interpret(C2)
            end;
        {Module, Function} ->
            case {apply(Module, immed, [Function, dummy]), Compile} of
                {false, true} ->
                    interpret(comma(Module, Function, C2));
                _ ->
                    interpret(apply(Module, Function, [C2]))
            end
    end.

interpret() ->
    interpret(load(#context{s=["ren.fth"]})).
i() ->
    interpret().
