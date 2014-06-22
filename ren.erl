-module(ren).
-compile(export_all).
-include("ren.hrl").

immed(';', _)           -> true;
immed('[\']', _)        -> true;
immed('\'', _)          -> true;
immed(postpone, _)      -> true;
immed('"', _)           -> true;
immed('(', _)           -> true;
immed(')', _)           -> true;
immed('[[', _)          -> true;
immed(']]', _)          -> true;
immed(_, _) ->
    false.

immed(Atom) ->
    M = module_of(Atom),
    try
        apply(M, immed, [Atom, dummy])
    catch
        error:_ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nop(C) -> C.

true(#context{s=S}=C) ->
    C#context{s=[true|S]}.

false(#context{s=S}=C) ->
    C#context{s=[false|S]}.

':'(#context{}=C) ->
    {Word, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[Word|S]}),
    C2#context{compile=true}.

';'(C) ->
    ';'(C, false).
immediate(C) ->
    ';'(C, true).
';'(#context{here=H, latest={atom, Line, Word}, debug=Debug}=C, Immed) ->
    C0 = gen_var(0),
    Clauses = make_clauses(lists:reverse(H), C0, 0),
    M = module_of(Word),
    Codes = [{attribute, Line, module, M},
             {attribute, Line, export, [{Word, 1}, {immed, 2}]},
             {attribute,Line,record,
              {context,
               [{record_field,1,{atom,1,s},{nil,1}},
                {record_field,2,{atom,2,r},{nil,2}},
                {record_field,3,{atom,3,cp}},
                {record_field,4,{atom,4,compile},{atom,4,false}},
                {record_field,5,{atom,5,here}},
                {record_field,6,{atom,6,latest}},
                {record_field,7,
                 {atom,7,source},
                 {tuple,7,[{atom,7,standard_io},{nil,7},{integer,7,0}]}},
                {record_field,8,{atom,8,debug},{integer,8,0}}]}},
             {function, Line, immed, 2,
              [{clause, Line, [{atom, Line, Word}, {var, Line, '_'}], [], [{atom, Line, Immed}]}]},
             {function, Line, Word, 1, Clauses}],
    Debug > 0 andalso io:format("~p\n", [Codes]),
    {ok, CModule, CBin} = compile:forms(Codes),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false}.

'[['(C) ->
    C#context{compile=false}.

']]'(C) ->
    C#context{compile=true}.

'compile?'(#context{s=S, compile=Compile}=C) ->
    C#context{s=[Compile|S]}.

here(#context{s=S, here=H}=C) ->
    C#context{s=[H|S]}.

','(#context{s=[Word|S], here=H}=C) ->
    Type = if
               is_atom(Word) -> atom;
               is_integer(Word) -> integer;
               is_float(Word) -> float;
               true -> string
           end,
    C#context{s=S, here=[{Type, 0, Word}|H]}.

'[\']'(#context{here=H}=C) ->
    {{_, Line, _}=Word, C1} = word(C),
    C1#context{here=[Word, {atom, Line, lit}, {atom, Line, lit}|H]}.

'\''(#context{s=S, here=H, compile=Compile}=C) ->
    {{_, Line, Word}=W, C2} = word(C),
    case Compile of
        true ->
            C2#context{here=[W, {atom, Line, lit}|H]};
        false ->
            C2#context{s=[Word|S]}
    end.

postpone(C) ->
    {Word, C1} = word(C),
    comma(Word, C1).

call(#context{s=[{Module, Function, Arity}|S]}=C) ->
    {Args, Rest} = lists:split(Arity, S),
    Ret = apply(Module, Function, lists:reverse(Args)),
    C#context{s=[Ret|Rest]};
call(#context{s=[Word|S]}=C) when is_atom(Word) ->
    M = module_of(Word),
    apply(M, Word, [C#context{s=S}]);
call(#context{s=[Block|S]}=C) ->
    call_block(Block, C#context{s=S}).


'=='(#context{s=[A,B|S]}=C) ->
    C#context{s=[B == A|S]}.

'/='(#context{s=[A,B|S]}=C) ->
    C#context{s=[B /= A|S]}.

'=<'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B =< A|S]}.

'<'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B < A|S]}.

'>='(#context{s=[A,B|S]}=C) ->
    C#context{s=[B >= A|S]}.

'>'(#context{s=[A,B|S]}=C) ->
    C#context{s=[B > A|S]}.

'=:='(#context{s=[A,B|S]}=C) ->
    C#context{s=[B =:= A|S]}.

'=/='(#context{s=[A,B|S]}=C) ->
    C#context{s=[B =/= A|S]}.


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

'.s'(#context{s=S}=C) ->
    print_list(S),
    io:nl(),
    C.

format(#context{s=[Args,Format|T]}=C) ->
    io:format(Format, Args),
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
    #context{s=[X|S]}=C2 = key(C),
    '"'(C2#context{s=S}, X, []).
'"'(#context{s=S, compile=Compile}=C, $", Acc) ->                %" %
    Str = lists:reverse(Acc),
    case Compile of
        true ->
            comma({string, 0, Str}, C);
        false ->
            C#context{s=[Str|S]}
    end;
'"'(C, $\\, Acc) ->
    #context{s=[X2|S2]}=C2 = key(C),
    #context{s=[X3|S3]}=C3 = key(C2#context{s=S2}),
    '"'(C3#context{s=S3}, X3, [backslash_char(X2)|Acc]);
'"'(C, X, Acc) ->
    #context{s=[X2|S2]}=C2 = key(C),
    '"'(C2#context{s=S2}, X2, [X|Acc]).

load(C) ->
    push_source(C).

'='(C) -> C.                                    %dummy definition

'case'(C) -> C.                                 %dummy definition
';case'(C) -> C.                                %dummy definition

'('(#context{r=R, compile=Compile, here=H}=C) ->
         C#context{r=[{Compile, H}|R], compile=true, here=[]}.

')'(C) -> right_paren(C, []).

right_paren(#context{s=S, r=[{Compile, H}|R], here=[]}=C, Acc) ->
    case Compile of
        false ->
            Block = right_paren_collect(Acc),
            C#context{s=[Block|S], r=R, compile=Compile, here=H};
        true ->
            C#context{r=R, compile=Compile, here=[{block, 0, Acc}|H]}
    end;
right_paren(#context{here=[H|T]}=C, Acc) ->
    right_paren(C#context{here=T}, [H|Acc]).
right_paren_collect([]) ->
    [];
right_paren_collect([{block, _, Block}|T]) ->
    [right_paren_collect(Block)|right_paren_collect(T)];
right_paren_collect([{atom, _, lit}, {_, _, Value}|T]) ->
    [Value|right_paren_collect(T)];
right_paren_collect([{_, _, Value}|T]) ->
    [Value|right_paren_collect(T)].




'>fun/0'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun() ->
                         handle_fun_result(call(#context{s=[Block]}))
                 end | T]}.
'>fun/1'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun(Arg1) ->
                         handle_fun_result(call(#context{s=[Block, Arg1]}))
                 end | T]}.
'>fun/2'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun(Arg1, Arg2) ->
                         handle_fun_result(call(#context{s=[Block, Arg2, Arg1]}))
                 end | T]}.
'>fun/3'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3) ->
                         handle_fun_result(call(#context{s=[Block, Arg3, Arg2, Arg1]}))
                 end | T]}.
'>fun/4'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3, Arg4) ->
                         handle_fun_result(call(#context{s=[Block, Arg4, Arg3, Arg2, Arg1]}))
                 end | T]}.
'>fun/5'(#context{s=[Block|T]}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3, Arg4, Arg5) ->
                         handle_fun_result(call(#context{s=[Block, Arg5, Arg4, Arg3, Arg2, Arg1]}))
                 end | T]}.
handle_fun_result(#context{s=[]}) ->
    [];
handle_fun_result(#context{s=[Result]}) ->
    Result;
handle_fun_result(#context{s=S}) ->
    S.


'self'(#context{s=S}=C) ->
    C#context{s=[self()|S]}.

spawn(C) ->
    #context{s=[Fun|T]}=C1 = '>fun/0'(C),
    C1#context{s=[erlang:spawn(Fun)|T]}.

send(#context{s=[Msg,Dest|T]}=C) ->
    erlang:send(Dest, Msg),
    C#context{s=T}.

'receive'(C) -> C.                              %dummy definition
'after'(C) -> C.                                %dummy definition
';receive'(C) -> C.                             %dummy definition

key(#context{s=S, source={In, [X|XS], Line}}=C) ->
    C#context{s=[X|S], source={In, XS, Line}};
key(C) ->
    key(refill(C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backslash_char($n) ->
    $\n;
backslash_char($r) ->
    $\r;
backslash_char($t) ->
    $\t;
backslash_char($v) ->
    $\v;
backslash_char(X) ->
    X.

module_of(Atom) ->
    case erlang:function_exported(ren, Atom, 1) of
        true ->
            ren;
        false ->
            list_to_atom("ren_" ++ atom_to_list(Atom))
    end.

push_source(#context{s=[File|S], r=R, source=Src}=C) ->
    {ok, In} = file:open(File, read),
    C#context{s=S, r=[Src|R], source={In, [], 0}}.

pop_source(#context{source={In, _, _}, r=[Src|T]}=C) ->
    file:close(In),
    C#context{source=Src, r=T}.

refill(#context{s=S, source={In, _, Line}}=C) ->
    case In of
        standard_io ->
            print_list(S),
            io:nl();
        _ ->
            ok
    end,
    case io:get_line(In, "") of
        eof ->
            pop_source(C);
        Buffer ->
            C#context{source={In, Buffer, Line + 1}}
    end.


word(C) ->
    #context{s=[X|S]}=C2 = key(C),
    C3 = C2#context{s=S},
    if
        X < 33 ->
            word(C3);
        true ->
            word([X], C3)
    end.

word(Word, C) ->
    #context{s=[X|S]}=C2 = key(C),
    C3 = C2#context{s=S},
    if
        X < 33 ->
            {parse_word(lists:reverse(Word), C3), C3};
        true ->
            word([X|Word], C3)
    end.

parse_word([H|_]=Word, #context{source={_, _, Line}}) when H == $_ ; H > $A - 1, H < $Z + 1 ->
    {var, Line, list_to_atom(Word)};
parse_word(Word, #context{source={_, _, Line}}) ->
    case string:to_integer(Word) of
        {Integer, []} -> {integer, Line, Integer};
        _ ->
            case string:to_float(Word) of
                {Float, []} -> {float, Line, Float};
                _ ->
                    case erlang_function(Word) of
                        {_Module, _Function, _Arity}=ErlangFunction ->
                            {erlang, Line, ErlangFunction};
                        _ ->
                            {atom, Line, list_to_atom(Word)}
                    end
            end
    end.

erlang_function(Word) ->
    case re:run(Word, "\\A(.+):(.+)/(.+)\\z", [{capture, [1, 2, 3], list}]) of
        {match, [Module, Function, Arity]} ->
            case string:to_integer(Arity) of
                {Int, []} ->
                    {list_to_atom(Module), list_to_atom(Function), Int};
                _ ->
                    false
            end;
        _ ->
            false
    end.


comma(X, #context{here=H}=C) ->
    C#context{here=[X|H]}.

lit(Literal, #context{s=S}=C) ->
    C#context{s=[Literal|S]}.

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
    gen_var(N, "X").
gen_var(N, Var) ->
    list_to_atom("# " ++ Var ++ integer_to_list(N)).

make_receive_clauses([{atom, _, ';receive'}|T], _, N, Acc) ->
    {lists:reverse(Acc), T, N};
make_receive_clauses([{atom, _, 'after'}, Milliseconds, Block|T], C, N, Acc) ->
    {Body, _, N1} = make_one_clause(Block, [], C, N),
    {lists:reverse(Acc), Milliseconds, Body, T, N1};
make_receive_clauses(Codes, C, N, Acc) ->
    {Pattern, [{block, _, Block}|Codes2]} = make_pattern(Codes),
    {Body, _, N2} = make_one_clause(Block, [], C, N),
    make_receive_clauses(Codes2, C, N2,
                      [{clause, 0, [Pattern], [], Body}|Acc]).

make_case_clauses([{atom, _, ';case'}|T], _, N, Acc) ->
    {lists:reverse(Acc), T, N};
make_case_clauses(Codes, C, N, Acc) ->
    {Pattern, [{block, _, Block}|Codes2]} = make_pattern(Codes),
    {Body, _, N2} = make_one_clause(Block, [], C, N),
    make_case_clauses(Codes2, C, N2,
                      [{clause, 0, [Pattern], [], Body}|Acc]).


make_pattern([{atom, Line, '[]'}|T]) ->
    {{nil, Line}, T};
make_pattern([{atom, _, '['}|T]) ->
    make_cons_pattern(T);
make_pattern([{atom, Line, '{'}|T]) ->
    {Elements, T1} = make_tupple_pattern(T, []),
    {{tuple, Line, Elements}, T1};
make_pattern([{_, _, _}=X|T]) ->
    {X, T}.

make_cons_pattern([{atom, Line, ']'}|T]) ->
    {{nil, Line}, T};
make_cons_pattern([X,{atom, _, '.]'}|T]) ->
    {X, T};
make_cons_pattern(Codes) ->
    {Car, Codes2} = make_pattern(Codes),
    {Cdr, Codes3} = make_cons_pattern(Codes2),
    {{cons, 0, Car, Cdr}, Codes3}.

make_tupple_pattern([{atom, _, '}'}|T], Acc) ->
    {lists:reverse(Acc), T};
make_tupple_pattern(Codes, Acc) ->
    {Element, Codes1} = make_pattern(Codes),
    make_tupple_pattern(Codes1, [Element|Acc]).


read_pattern({Word, C}) ->
    case Word of
        {atom, Line, '[]'} ->
            {{nil, Line}, C};
        {atom, _, '['} ->
            read_cons_pattern(word(C));
        {atom, Line, '{'} ->
            read_tupple_pattern(word(C), {nil, Line});
        X ->
            {X, C}
    end.

read_cons_pattern({{atom, Line, ']'}, C}) ->
    {{nil, Line}, C};
read_cons_pattern({{atom, _, '.]'}, C}) ->
    {C};
read_cons_pattern({{_, Line, _}=Word, C}) ->
    {Car, C1} = read_pattern({Word, C}),
    case read_cons_pattern(word(C1)) of
        {Cdr, C2} ->
            {{cons, Line, Car, Cdr}, C2};
        {C2} ->
            {Car, C2}
    end.

read_tupple_pattern({{atom, Line, '}'}, C}, Elements) ->
    {{tuple, Line, Elements}, C};
read_tupple_pattern({Word, C}, Elements) ->
    {Pattern, C1} = read_pattern({Word, C}),
    read_tupple_pattern(word(C1), [Pattern|Elements]).


make_clauses(Codes, C, N) ->
    {Arg, Codes1, N1} = make_clause_arg(Codes, C, N),
    {Acc, C3, N3} = case N of
                        N1 ->
                            {[], C, N};
                        _ ->
                            C1 = gen_var(N1),
                            N2 = N1 + 1,
                            C2 = gen_var(N2),
                            {[{match,1,
                               {var,1, C2},
                               {record,1,
                                {var,1, C},
                                      context,
                                [{record_field,1,{atom,1,s},{var,1,C1}}]}}],
                             C2, N2}
                    end,
    {Clause, Codes2, _} = make_one_clause(Codes1, Acc, C3, N3),
    [{clause, 0,
      Arg,
      [],
      Clause}|case Codes2 of
                  [] -> [];
                  _ -> make_clauses(Codes2, C, N)
              end].

make_clause_arg([{atom, Line, '(('}|T], C, N) ->
    {Pattern, T1} = make_clause_arg_pattern(T, {var, Line, gen_var(N+1)}),
    {[{match, Line,
      {record, Line, context,
       [{record_field, Line,
         {atom, Line, s},
         Pattern}]},
      {var, Line, C}}],
     T1,
     N + 1};
make_clause_arg(Codes, C, N) ->
    {[{var, 0, C}], Codes, N}.

make_clause_arg_pattern([{atom, _, '))'}|T], Acc) ->
    {Acc, T};
make_clause_arg_pattern([{_, Line, _}|_]=Codes, Acc) ->
    {Pattern, Codes1} = make_pattern(Codes),
    make_clause_arg_pattern(Codes1, {cons, Line, Pattern, Acc}).


end_make_one_clause(Codes, [], C, N) ->
    {[{var, 0, C}], Codes, N};         % : a ; みたいに本体が空の場合
end_make_one_clause(Codes, Acc, _, N) ->
    Clauses = lists:flatten(lists:reverse(Acc)),
    %% :2: Warning: variable '#__C__2__' is unused が出ないように
    %% 最後の match を削除する。
    [{match, _, _, LastClause}|T] = lists:reverse(Clauses),
    {lists:reverse([LastClause|T]), Codes, N-1}.

make_one_clause([], Acc, C, N) ->
    end_make_one_clause([], Acc, C, N);
make_one_clause([{atom, Line, lit}, Literal|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    make_one_clause(T,
                    [{match, Line,
                      {var, Line, C1},
                      {call, Line,
                       {remote, Line, {atom, Line, ren}, {atom, Line, lit}},
                       [Literal,
                        {var, Line, C}]}} | Acc],
                    C1,
                    N + 1);
make_one_clause([{atom, _, '(('}|_]=T, Acc, C, N) ->
    end_make_one_clause(T, Acc, C, N);
make_one_clause([{atom, _, '='}|T], Acc, C, N) ->
    {Pattern, T2} = make_pattern(T),
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    make_one_clause(T2,
                 [[{match, 1,
                    {cons, 1, {var, 1, SH}, {var, 1, ST}},
                    {record_field, 1, {var, 1, C}, context, {atom, 1, s}}},
                   {match, 2, Pattern, {var, 2, SH}},
                   {match, 3,
                    {var, 3, C2},
                    {record, 3,
                     {var, 3, C},
                     context,
                     [{record_field, 3, {atom, 3, s}, {var, 3, ST}}]}}] | Acc],
                 C2,
                 N+3);
make_one_clause([{atom, Line, 'receive'}|T], Acc, C, N) ->
    {Receive, T2, N2} =
        case make_receive_clauses(T, C, N, []) of
            {ReceiveClauses, T1, N1} ->
                {{'receive', Line, ReceiveClauses}, T1, N1};
            {ReceiveClauses, Milliseconds, AfterClauses, T1, N1} ->
                {{'receive', Line, ReceiveClauses, Milliseconds, AfterClauses}, T1, N1}
        end,
    C3 = gen_var(N2 + 1),
    make_one_clause(T2,
                 [{match, Line, {var, Line, C3}, Receive}|Acc],
                 C3,
                 N2 + 1);
make_one_clause([{atom, Line, 'case'}|T], Acc, C, N) ->
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    {CaseClauses, T2, N2} = make_case_clauses(T, C2, N+3, []),
    C3 = gen_var(N2 + 1),
    make_one_clause(T2,
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
make_one_clause([{atom, Line, Function}|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    Module = module_of(Function),
    make_one_clause(T,
                 [{match, Line,
                   {var, Line, C1},
                   {call, Line,
                    {remote, Line, {atom, Line, Module}, {atom, Line, Function}},
                    [{var, Line, C}]}} | Acc],
                 C1,
                 N + 1);
make_one_clause([{block, Line, Block}|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    List = block_to_list(Block, Line),
    make_one_clause(T,
                 [{match, Line,
                   {var, Line, C1},
                   {call, Line,
                    {remote, Line, {atom, Line, ren}, {atom, Line, lit}},
                    [List,
                     {var, Line, C}]}} | Acc],
                 C1,
                 N + 1);
make_one_clause([{erlang, Line, {Module, Function, Arity}}|T], Acc, C, N) ->
    Args = gen_var(N+1),
    Rest = gen_var(N+2),
    C1 = gen_var(N+3),
    make_one_clause(T,
                 [[{match, Line,
                    {tuple, Line, [{var, Line, Args}, {var, Line, Rest}]},
                    {call, Line,
                     {remote, Line, {atom, Line, lists}, {atom, Line, split}},
                     [{integer, Line, Arity},
                      {record_field, Line, {var, Line, C}, context, {atom, Line, s}}]}},
                   {match, Line,
                    {var, Line, C1},
                    {record, Line,
                     {var, Line, C},
                     context,
                     [{record_field, Line,
                       {atom, Line, s},
                       {cons, Line,
                        {call, Line,
                         {atom, Line, apply},
                         [{atom, Line, Module},
                          {atom, Line, Function},
                          {call, Line,
                           {remote, Line, {atom, Line, lists}, {atom, Line, reverse}},
                           [{var, Line, Args}]}]},
                        {var, Line, Rest}}}]}}] | Acc],
                 C1,
                 N + 3);
make_one_clause([{Type, Line, Value}|T], Acc, C, N) ->
    C1 = gen_var(N+1),
    make_one_clause(T,
                 [{match, 0,
                   {var, 0, C1},
                   {call, 0,
                    {remote, 1, {atom, 1, ren}, {atom, 1, lit}},
                    [{Type, Line, Value},
                     {var, 1, C}]}} | Acc],
                 C1,
                 N + 1).

block_to_list([], Line) ->
    {nil, Line};
block_to_list([{block, Line2, Block}|T], Line) ->
    {cons, Line, block_to_list(Block, Line2), block_to_list(T, Line)};
block_to_list([{atom, _, lit},Literal|T], Line) ->
    {cons, Line, Literal, block_to_list(T, Line)};
block_to_list([H|T], Line) ->
    {cons, Line, H, block_to_list(T, Line)}.

header(#context{s=[Word|S]}=C) ->
    C#context{s=S,
              latest=Word,
              here=[]}.

call_block([], C) ->
    C;
call_block([lit, Literal|T], #context{s=S}=C) ->
    call_block(T, C#context{s=[Literal|S]});
call_block([{Module, Function, Arity}|T], #context{s=S}=C) ->
    {Args, Rest} = lists:split(Arity, S),
    Ret = apply(Module, Function, lists:reverse(Args)),
    call_block(T, C#context{s=[Ret|Rest]});
call_block([H|T], C) when is_atom(H) ->
    Module = module_of(H),
    call_block(T, apply(Module, H, [C]));
call_block([H|T], #context{s=S}=C) ->
    call_block(T, C#context{s=[H|S]}).

print_list(List) ->
    print_list(lists:reverse(List), first).
print_list([], _) ->
    ok;
print_list([X|XS], first) ->
    io:format("~tp", [X]),
    print_list(XS, rest);
print_list([X|XS], _) ->
    io:format(" ~tp", [X]),
    print_list(XS, " ").


eval({var, _, _}=Expr, #context{s=S}=C, B) ->
    {value, Value, B1} = erl_eval:expr(Expr, B),
    {C#context{s=[Value|S]}, B1};
eval({atom, Line, '='}, #context{s=[Value|S]}=C, B) ->
    {Pattern, C1} = read_pattern(word(C#context{s=S})),
    B1 = erl_eval:add_binding('# Value', Value, B),
    Expr = {match, Line, Pattern, {var,  Line, '# Value'}},
    {value, _, B2} = erl_eval:expr(Expr, B1),
    {C1, B2};
eval({atom, Line, Atom}=Function, C, B) ->
    B1 = erl_eval:add_binding('# C', C, B),
    Module = module_of(Atom),
    Expr = {call, Line,
            {remote, Line, {atom, Line, Module}, Function},
            [{var, Line, '# C'}]},
    {value, C2, B2} = erl_eval:expr(Expr, B1),
    {C2, B2};
eval({erlang, Line, {Module, Function, Arity}}, #context{s=S}=C, B) ->
    {Args, Rest} = lists:split(Arity, S),
    C1 = C#context{s=Rest},
    B1 = erl_eval:add_binding('# C', C1, B),
    B2 = erl_eval:add_binding('# Args', lists:reverse(Args), B1),
    Expr = {call, Line,
            {remote, Line, {atom, Line, erlang}, {atom, Line, apply}},
            [{atom, Line, Module}, {atom, Line, Function}, {var, Line, '# Args'}]},
    {value, Value, B3} = erl_eval:expr(Expr, B2),
    {C1#context{s=[Value|Rest]}, B3}.


interpret(#context{s=S, r=R, compile=Compile, here=H, debug=Debug}=C, B) ->
    case Debug > 0 of
        true ->
            io:format("s: "),
            print_list(S),
            io:format(" r: "),
            print_list(R),
            io:format(" h: "),
            print_list(H),
            io:format(" c: ~p\n", [Compile]);
        _ ->
            ok
    end,
    case word(C) of
        {{var, _, _}=Var, C2} ->
            case Compile of
                false ->
                    {C3, B3} = eval(Var, C2, B),
                    interpret(C3, B3);
                true ->
                    interpret(comma(Var, C2), B)
            end;
        {{Type, _, N}=Number, C2} when Type == integer; Type == float ->
            case Compile of
                false ->
                    interpret(C2#context{s=[N|S]}, B);
                true ->
                    interpret(comma(Number, C2), B)
            end;
        {{erlang, _, {_Module, _Function, _Arity}}=ErlangFunction, #context{s=S}=C2} ->
            case Compile of
                true ->
                    interpret(comma(ErlangFunction, C2), B);
                _ ->
                    {C3, B3} = eval(ErlangFunction, C2, B),
                    interpret(C3, B3)
            end;
        {{atom, _, A}=Atom, #context{s=S}=C2} ->
            case {immed(A), Compile} of
                {false, true} ->
                    interpret(comma(Atom, C2), B);
                _ ->
                    {C3, B3} = eval(Atom, C2, B),
                    interpret(C3, B3)
            end
    end.


i() ->
    interpret(load(#context{s=["ren.fth"]}), erl_eval:new_bindings()).

d() ->
    interpret(load(#context{s=["ren.fth"], debug=1}), erl_eval:new_bindings()).

test() ->
    C1 = load(#context{s=["test.fth"]}),
    C2 = load(C1#context{s=["ren.fth"]}),
    interpret(C2, erl_eval:new_bindings()).
