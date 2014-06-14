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
    Clauses = make_clauses(lists:reverse(H), [], 0),
    {M, F} = module_function(W),
    Codes = [{attribute, 0, module, M},
             {attribute, 0, export, [{F, 1}, {immed, 2}]},
             {attribute,0,record,
              {context,
               [{record_field,1,{atom,1,s},{nil,1}},
                {record_field,2,{atom,2,r},{nil,2}},
                {record_field,3,{atom,3,cp}},
                {record_field,4,{atom,4,compile},{atom,4,false}},
                {record_field,5,{atom,5,here}},
                {record_field,6,{atom,6,latest}},
                {record_field,7,{atom,7,d}},
                {record_field,8,{atom,8,buffer},{string,8,[]}},
                {record_field,9,{atom,9,source},{atom,9,standard_io}}]}},
             {function, 0, immed, 2,
              [{clause, 0, [{atom, 0, F}, {var, 0, '_'}], [], [{atom, 0, false}]}]},
             {function, 0, F, 1, [{clause, 0, [{var, 0, gen_var(0)}], [],
                                   Clauses}]}],
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
'"'(C, $\\, Acc) ->
    {X2,  C2} = key(C),
    {X3,  C3} = key(C2),
    '"'(C3, X3, [X2|Acc]);
'"'(C, X, Acc) ->
    {X2,  C2} = key(C),
    '"'(C2, X2, [X|Acc]).

load(C) ->
    push_source(C).

'='(C) ->                                       %dummy definition
    C.

%'case'(#context{}=C) ->
%    case_(C).
%case_(C) ->
%    {Word, C2} = word(C),
%    case_(C, Word, []).
%case_(C, ";case", Acc) ->
%    {X, XS} = lists:splitwith(fun(X) -> X =/= ";;" end, lists:reverse(Acc)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_function(Word) when is_atom(Word) ->
    module_function(atom_to_list(Word));
module_function(Word) ->
    {list_to_atom("forth_" ++ Word),
     list_to_atom(Word)}.

find([H|_]=W, _) when H == $_ ; H > $A - 1, H < $Z + 1 ->
    {var, list_to_atom(W)};
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

comma({var, _}=V, #context{here=H}=C) ->
    C#context{here=[V|H]};
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

gen_var(N) ->
    list_to_atom("__C__" ++ integer_to_list(N) ++ "__").

make_clauses([], Acc, _) ->
    lists:flatten(lists:reverse(Acc));
make_clauses([{ren, '='}, {var, Var}|T], Acc, N) ->
    C = gen_var(N),
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    make_clauses(T,
                 [[{match, 1,
                    {cons, 1, {var, 1, SH}, {var, 1, ST}},
                    {record_field, 1, {var, 1, C}, context, {atom, 1, s}}},
                   {match, 2, {var, 2, Var}, {var, 2, SH}},
                   {match, 3,
                    {var, 3, C2},
                    {record, 3,
                     {var, 3, C},
                     context,
                     [{record_field, 3, {atom, 3, s}, {var, 3, ST}}]}}] | Acc],
                 N+3);
make_clauses([{lit, Literal}|T], Acc, N) ->
    make_clauses(T,
                 [{match, 0,
                   {var, 0, gen_var(N+1)},
                   {call, 0,
                    {remote, 1, {atom, 1, ren}, {atom, 1, lit}},
                    [{literal_type(Literal), 1, Literal},
                     {var, 1, gen_var(N)}]}} | Acc],
                N + 1);
make_clauses([{var, Var}|T], Acc, N) ->
    make_clauses(T,
                 [{match, 0,
                   {var, 0, gen_var(N+1)},
                   {call, 0,
                    {remote, 1, {atom, 1, ren}, {atom, 1, lit}},
                    [{var, 1, Var},
                     {var, 1, gen_var(N)}]}} | Acc],
                N + 1);
make_clauses([{Module, Function}|T], Acc, N) ->
    make_clauses(T,
                 [{match, 0,
                   {var, 0, gen_var(N+1)},
                   {call, 1,
                    {remote, 1, {atom, 1, Module}, {atom, 1, Function}},
                    [{var, 1, gen_var(N)}]}} | Acc],
                 N + 1).

header(#context{s=[Word|S]}=C) ->
    C#context{s=S,
              latest=Word,
              here=[]}.

interpret(#context{s=S, r=R, compile=Compile, here=H}=C) ->
    io:format("next: s=~w r=~w h=~w\n", [S, R, H]),
    {Word, C2} = word(C),
    case find(Word, C) of
        {var, Var} ->
            case Compile of
                false ->
                    io:format("var ~s is unknow.\n", [Var]),
                    interpret(C2);
                true ->
                    interpret(comma({var, Var}, C2))
            end;
        {Module, Function} ->
            case {apply(Module, immed, [Function, dummy]), Compile} of
                {false, true} ->
                    interpret(comma(Module, Function, C2));
                _ ->
                    interpret(apply(Module, Function, [C2]))
            end;
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
            end
    end.

interpret() ->
    interpret(load(#context{s=["ren.fth"]})).
i() ->
    interpret().
