-module(ren).
-compile(export_all).
-include("ren.hrl").

immed(';', _) ->
    true;
immed('"', _) ->                                %"
    true;
immed(_, _) ->
    false.

immed(Atom) ->
    M = module_of(Atom),
    apply(M, immed, [Atom, dummy]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

true(#context{s=S}=C) ->
    C#context{s=[true|S]}.

false(#context{s=S}=C) ->
    C#context{s=[false|S]}.

':'(#context{}=C) ->
    {Word, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[Word|S]}),
    C2#context{compile=true}.

';'(#context{here=H, latest={atom, _, Word}}=C) ->
    {Clauses, _} = make_clauses(lists:reverse(H), [], gen_var(0), 0),
    M = module_of(Word),
    Codes = [{attribute, 0, module, M},
             {attribute, 0, export, [{Word, 1}, {immed, 2}]},
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
                {record_field,9,{atom,9,source},{atom,9,standard_io}},
                {record_field,10,{atom,10,line},{integer,10,0}}]}},
             {function, 0, immed, 2,
              [{clause, 0, [{atom, 0, Word}, {var, 0, '_'}], [], [{atom, 0, false}]}]},
             {function, 0, Word, 1, [{clause, 0, [{var, 0, gen_var(0)}], [],
                                   Clauses}]}],
    io:format("~p\n", [Codes]),
    {ok, CModule, CBin} = compile:forms(Codes),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false}.

'\''(#context{s=S}=C) ->
    {{_, _, Word}, C2} = word(C),
    C2#context{s=[Word|S]}.

call(#context{s=[Word|S]}=C) ->
    M = module_of(Word),
    apply(M, Word, [C#context{s=S}]).

'+'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B+A|S]}.

'-'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B-A|S]}.

'*'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B*A|S]}.

'/'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B/A|S]}.

'.'(#context{s=[H|T]}=C) ->
    case io_lib:printable_unicode_list(H) of
        true ->
            io:format("~ts\n", [H]);
        false ->
            io:format("~tp\n", [H])
    end,
    C#context{s=T}.

'['(#context{s=S}=C) ->
    C#context{s=['['|S]}.

']'(#context{s=S}=C) ->
    C#context{s=']'(S, [])}.
']'(['['|T], List) ->
    [List|T];
']'([H|T], List) ->
    ']'(T, [H|List]).

'.]'(#context{s=[H|T]}=C) ->
    C#context{s=']'(T, H)}.

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
            comma({string, 0, Str}, C);
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

'='(C) -> C.                                    %dummy definition

'case'(C) -> C.                                 %dummy definition
'->'(C) -> C.                                   %dummy definition
';;'(C) -> C.                                   %dummy definition
';case'(C) -> C.                                %dummy definition


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_of(Atom) ->
    case erlang:function_exported(ren, Atom, 1) of
        true ->
            ren;
        false ->
            list_to_atom("ren_" ++ atom_to_list(Atom))
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
            {parse_word(lists:reverse(Word), C2), C2};
        {X, C2} ->
            word([X|Word], C2)
    end.

parse_word([H|_]=Word, #context{line=L}) when H == $_ ; H > $A - 1, H < $Z + 1 ->
    {var, L, list_to_atom(Word)};
parse_word(Word, #context{line=L}) ->
    case string:to_integer(Word) of
        {N, []} -> {integer, L, N};
        _ ->
            case string:to_float(Word) of
                {N, []} -> {float, L, N};
                _ ->
                    {atom, L, list_to_atom(Word)}
            end
    end.


comma(X, #context{here=H}=C) ->
    C#context{here=[X|H]}.

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
    list_to_atom("#__C__" ++ integer_to_list(N) ++ "__").


make_case_clauses([{atom, _, ';case'}|T], _, N, Acc) ->
    {lists:reverse(Acc), T, N};
make_case_clauses(Codes, C, N, Acc) ->
    {Pattern, Codes2} = make_pattern(Codes),
    {Body, Codes3, N3} = make_case_body(Codes2, C, N, []),
    make_case_clauses(Codes3, C, N3,
                      [{clause, 0, [Pattern], [], Body}|Acc]).

make_pattern([{var, Line, Var}|T]) ->
    {{var, Line, Var}, T};
make_pattern([{atom, _, '['}|T]) ->
    make_cons_pattern(T);
make_pattern([{atom, _, '{'}|T]) ->
    make_tupple_pattern(T);
make_pattern([{atom, Line, Atom}|T]) ->
    {{atom, Line, Atom}, T}.

make_cons_pattern([{atom, Line, ']'}|T]) ->
    {{nil, Line}, T};
make_cons_pattern([X,{atom, _, '.]'}|T]) ->
    {X, T};
make_cons_pattern(Codes) ->
    {Car, Codes2} = make_pattern(Codes),
    {Cdr, Codes3} = make_cons_pattern(Codes2),
    {{cons, 0, Car, Cdr}, Codes3}.

make_tupple_pattern(X) ->
    X.

make_case_body([{atom, _, ';;'}|T], _, N, Acc) ->
    {lists:flatten(lists:reverse(Acc)), T, N};
make_case_body([{atom, _, ';case'}|_]=Codes, _, N, Acc) ->
    {lists:flatten(lists:reverse(Acc)), Codes, N};
make_case_body([H|T], C, N, Acc) ->
    {Clause, N2} = make_clauses([H], [], C, N),
    make_case_body(T, gen_var(N2), N2, [Clause|Acc]).

make_clauses([], Acc, _, N) ->
    {lists:flatten(lists:reverse(Acc)), N};
make_clauses([{atom, _, '='}, Var|T], Acc, C, N) ->
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    make_clauses(T,
                 [[{match, 1,
                    {cons, 1, {var, 1, SH}, {var, 1, ST}},
                    {record_field, 1, {var, 1, C}, context, {atom, 1, s}}},
                   {match, 2, Var, {var, 2, SH}},
                   {match, 3,
                    {var, 3, C2},
                    {record, 3,
                     {var, 3, C},
                     context,
                     [{record_field, 3, {atom, 3, s}, {var, 3, ST}}]}}] | Acc],
                 C2,
                 N+3);
make_clauses([{atom, Line, 'case'}|T], Acc, C, N) ->
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    {CaseClauses, T2, N2} = make_case_clauses(T, C2, N+3, []),
    C3 = gen_var(N2 + 1),
    make_clauses(T2,
                 [[{match, 1,
                    {cons, 1, {var, 1, SH}, {var, 1, ST}},
                    {record_field, 1, {var, 1, C}, context, {atom, 1, s}}},
                   {match, 3,
                    {var, 3, C2},
                    {record, 3,
                     {var, 3, C},
                     context,
                     [{record_field, 3, {atom, 3, s}, {var, 3, ST}}]}},
                   {match, Line,
                    {var, Line, C3},
                    {'case', 0,
                          {var, 0, SH},
                          CaseClauses}}] | Acc],
                 C3,
                 N2+1);
make_clauses([{atom, Line, Function}|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    Module = module_of(Function),
    make_clauses(T,
                 [{match, Line,
                   {var, Line, C1},
                   {call, Line,
                    {remote, Line, {atom, Line, Module}, {atom, Line, Function}},
                    [{var, Line, C}]}} | Acc],
                 C1,
                 N + 1);
make_clauses([{Type, Line, Value}|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    make_clauses(T,
                 [{match, 0,
                   {var, 0, C1},
                   {call, 0,
                    {remote, 1, {atom, 1, ren}, {atom, 1, lit}},
                    [{Type, Line, Value},
                     {var, 1, C}]}} | Acc],
                 C1,
                 N + 1).

header(#context{s=[Word|S]}=C) ->
    C#context{s=S,
              latest=Word,
              here=[]}.

interpret(#context{s=S, r=R, compile=Compile, here=H}=C) ->
    io:format("s=~w r=~w h=~w, c=~w\n", [S, R, H, Compile]),
    case word(C) of
        {{var, _, _}=Var, C2} ->
            case Compile of
                false ->
                    io:format("var ~s is invalide when an execute state.\n", [Var]),
                    interpret(C2);
                true ->
                    interpret(comma(Var, C2))
            end;
        {{Type, _, N}=Number, C2} when Type == integer; Type == float ->
            case Compile of
                false ->
                    interpret(C2#context{s=[N|S]});
                true ->
                    interpret(comma(Number, C2))
            end;
        {{atom, _, A}=Atom, #context{s=S}=C2} ->
            case {immed(A), Compile} of
                {false, true} ->
                    interpret(comma(Atom, C2));
                _ ->
                    interpret(call(C2#context{s=[A|S]}))
            end
    end.

interpret() ->
    interpret(load(#context{s=["ren.fth"]})).
i() ->
    interpret().
