-module(ren).
-compile(export_all).
-include("ren.hrl").

immed(';')      -> true;
immed('(')      -> true;
immed(')')      -> true;
immed('[[')     -> true;
immed(']]')     -> true;
immed(_)        -> false.

immed(Module, Word) ->
    try
        apply(Module, immed, [Word])
    catch
        error:_ -> false
    end.

immed(Current, Use, Word) ->
    {M, W} = module_word(Word, Current, Use),
    immed(M, W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% built in word
module(#context{s=[Module|S], source=Src}=C) ->
    C#context{s=S, source=Src#src{module=Module, use=[{core, core}, {biw, biw}]}}.

'use-module-as'(#context{s=[As,Module|S], source=#src{use=Use}=Src}=C) ->
    C#context{s=S, source=Src#src{use=[{Module, As}|Use]}}.

'current-module'(#context{s=S, source=#src{module=Current}}=C) ->
    C#context{s=[Current|S]}.

'dump-context'(#context{s=S}=C) ->
    C#context{s=[C|S]}.

'>r'(#context{s=[H|T], r=R}=C) ->
    C#context{s=T, r=[H|R]}.

'r>'(#context{s=S, r=[H|T]}=C) ->
    C#context{s=[H|S], r=T}.

'[]'(#context{s=S}=C) ->
    C#context{s=[[]|S]}.

cons(#context{s=[Y,X|T]}=C) ->
    C#context{s=[[X|Y]|T]}.

':'(C) ->
    {Word, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[Word|S]}),
    C2#context{compile=true}.

';'(C) ->
    semicolon(C, false).

immediate(C) ->
    semicolon(C, true).

':g'(C) ->
    {{atom, Line, Word}, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[{atom, Line, {g, Word}}|S]}),
    C2#context{compile=true}.

':m'(C) ->
    {{atom, Line, Word}, #context{s=S}=C1} = word(C),
    C2 = header(C1#context{s=[{atom, Line, {m, Word}}|S]}),
    C2#context{compile=true}.

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

'\''(#context{s=S}=C) ->
    {{_, _, Word}, C2} = word(C),
    C2#context{s=[Word|S]}.

call(#context{s=[{Module, Function, Arity}|S]}=C) ->
    {Args, Rest} = lists:split(Arity, S),
    Ret = apply(Module, Function, lists:reverse(Args)),
    C#context{s=[Ret|Rest]};
call(#context{s=[Word|S], source=#src{module=Current, use=Use}}=C) when is_atom(Word) ->
    {M, W} = module_word(Word, Current, Use),
    apply(M, W, [C#context{s=S}]);
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
right_paren_collect([{_, _, Value}|T]) ->
    [Value|right_paren_collect(T)].




'>fun/0'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun() ->
                         handle_fun_result(call(#context{s=[Block],
                                                         source=#src{module=Current, use=Use}}))
                 end | T]}.
'>fun/1'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun(Arg1) ->
                         handle_fun_result(call(#context{s=[Block, Arg1],
                                                         source=#src{module=Current, use=Use}}))
                 end | T]}.
'>fun/2'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun(Arg1, Arg2) ->
                         handle_fun_result(call(#context{s=[Block, Arg2, Arg1],
                                                         source=#src{module=Current, use=Use}}))
                 end | T]}.
'>fun/3'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3) ->
                         handle_fun_result(call(#context{s=[Block, Arg3, Arg2, Arg1],
                                                         source=#src{module=Current, use=Use}}))
                 end | T]}.
'>fun/4'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3, Arg4) ->
                         handle_fun_result(call(#context{s=[Block, Arg4, Arg3, Arg2, Arg1],
                                                         source=#src{module=Current, use=Use}}))
                 end | T]}.
'>fun/5'(#context{s=[Block|T], source=#src{module=Current, use=Use}}=C) ->
    C#context{s=[fun(Arg1, Arg2, Arg3, Arg4, Arg5) ->
                         handle_fun_result(call(#context{s=[Block, Arg5, Arg4, Arg3, Arg2, Arg1],
                                                         source=#src{module=Current, use=Use}}))
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

key(#context{s=S, source=#src{buffer=[X|XS]}=Src}=C) ->
    C#context{s=[X|S], source=Src#src{buffer=XS}};
key(C) ->
    key(refill(C)).


'clear-stack'(C) ->
    C#context{s=[]}.


'type-of'(#context{s=[X|S]}=C) ->
    Type = if
               is_integer(X)   -> integer;
               is_float(X)     -> float;
               is_list(X)      -> list;
               is_tuple(X)     -> tuple;
               is_map(X)       -> map;
               is_bitstring(X) -> bitstring;
               is_binary(X)    -> binary;
               is_boolean(X)   -> boolean;
               is_function(X)  -> function;
               is_pid(X)       -> pid;
               is_port(X)      -> port;
               is_reference(X) -> reference;
               is_atom(X)      -> atom;
               true            -> unknown
           end,
    C#context{s=[Type|S]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_word(Word, Module, Use) ->
    L = atom_to_list(Word),
    case lists:splitwith(fun(X) -> X =/= $. end, L) of
        {W, []} ->
            W2 = list_to_atom(W),
            M = module_of(W2, Module, Use),
            {real_module(M, W2), W2};
        {[], W} ->
            W2 = list_to_atom(W),
            M = module_of(W2, Module, Use),
            {real_module(M, W2), W2};
        {M, [$.|W]} ->
            M2 = list_to_atom(M),
            W2 = list_to_atom(W),
            {MM, M2} = lists:keyfind(M2, 2, Use),
            {real_module(MM, W2), W2}
    end.

module_of(_, Current, []) ->
    Current;
module_of(Atom, Current, [{X, _}|XS]) ->
    case erlang:function_exported(real_module(X, Atom), Atom, 1) of
        true ->
            X;
        false ->
            module_of(Atom, Current, XS)
    end.

real_module(Module, Word) ->
    case Module of
        biw ->
            ren;
        _ ->
            list_to_atom(lists:concat(['ren ', Module, ' ', Word]))
    end.



push_source(#context{s=[File|S], r=R, source=Src}=C) ->
    {ok, In} = file:open(File, read),
    C#context{s=S, r=[Src|R], source=#src{in=In}}.

pop_source(#context{source=#src{in=In}, r=[Src|T]}=C) ->
    file:close(In),
    C#context{source=Src, r=T}.

refill(#context{s=S, source=#src{in=In, line=Line}=Src}=C) ->
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
            C#context{source=Src#src{buffer=Buffer, line=Line + 1}}
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

parse_word([$',X|XS], #context{source=#src{line=Line}}) ->
    {quote, Line, list_to_atom([X|XS])};
parse_word([H|_]=Word, #context{source=#src{line=Line}}) when H == $_ ; H > $A - 1, H < $Z + 1 ->
    {var, Line, list_to_atom(Word)};
parse_word(Word, #context{source=#src{line=Line}}) ->
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
    exit(bye).

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

make_receive_clauses([{atom, _, ';receive'}|T], _, _, _, N, Acc) ->
    {lists:reverse(Acc), T, N};
make_receive_clauses([{atom, _, 'after'}, Milliseconds, Block|T], Current, Use, C, N, Acc) ->
    {Body, _, N1} = make_one_clause(Block, Current, Use, [], C, N),
    {lists:reverse(Acc), Milliseconds, Body, T, N1};
make_receive_clauses(Codes, Current, Use, C, N, Acc) ->
    {Pattern, [{block, _, Block}|Codes2]} = make_pattern(Codes),
    {Body, _, N2} = make_one_clause(Block, Current, Use, [], C, N),
    make_receive_clauses(Codes2, Current, Use, C, N2,
                         [{clause, 0, [Pattern], [], Body}|Acc]).

make_case_clauses([{atom, _, ';case'}|T], _, _, _, N, Acc) ->
    {lists:reverse(Acc), T, N};
make_case_clauses(Codes, Current, Use, C, N, Acc) ->
    {Pattern, [{block, _, Block}|Codes2]} = make_pattern(Codes),
    {Body, _, N2} = make_one_clause(Block, Current, Use, [], C, N),
    make_case_clauses(Codes2, Current, Use, C, N2,
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

%% :g
semicolon(#context{here=H, latest={atom, Line, {g, Word}}, debug=Debug,
             source=#src{module=Current, use=Use}}=C, Immediate) ->
    {M, W} = module_word(Word, Current, Use),
    Codes = default_codes(M, Immediate) ++ [default_dispatch_code()],
    C0 = gen_var(0),
    Clauses = make_clauses(lists:reverse([{atom, Line, 'm dispatch'}|H]), Current, Use, C0, 0),
    Codes2 = add_function(Codes, W, Line, Clauses),
    Debug > 0 andalso io:format("~p\n", [Codes2]),
    {ok, CModule, CBin} = compile:forms(update_codes_attribute(Codes2)),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false};
%% :m
semicolon(#context{here=H, latest={atom, Line, {m, Word}}, debug=Debug,
             source=#src{module=Current, use=Use}}=C, _) ->
    {M, _} = module_word(Word, Current, Use),
    Codes = retrieve_codes(M),
    C0 = gen_var(0),
    Clauses = make_clauses(lists:reverse(H), Current, Use, C0, 0),
    Codes2 = add_method(Codes, Line, Clauses),
    Debug > 0 andalso io:format("~p\n", [Codes2]),
    {ok, CModule, CBin} = compile:forms(update_codes_attribute(Codes2)),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false};
%% :
semicolon(#context{here=H, latest={atom, Line, Word}, debug=Debug,
             source=#src{module=Current, use=Use}}=C, Immediate) ->
    {M, W} = module_word(Word, Current, Use),
    Codes = default_codes(M, Immediate),
    C0 = gen_var(0),
    Clauses = make_clauses(lists:reverse(H), Current, Use, C0, 0),
    Codes2 = add_function(Codes, W, Line, Clauses),
    Debug > 0 andalso io:format("~p\n", [Codes2]),
    {ok, CModule, CBin} = compile:forms(Codes2),
    code:load_binary(CModule, atom_to_list(CModule), CBin),
    C#context{compile=false}.

default_codes(Module, Immediate) ->
    Line = 0,
    [{attribute, Line, module, Module},
     {attribute, Line, export, [{immed, 1}]},
     {attribute, Line, ren_codes, []},
     {attribute,0,record,
      {src,
       [{record_field,1,{atom,1,in},{atom,1,standard_io}},
        {record_field,2,{atom,2,buffer},{nil,2}},
        {record_field,3,{atom,3,line},{integer,3,0}},
        {record_field,4,{atom,4,use},{cons,4,{tuple,4,[{atom,4,biw},{atom,4,biw}]},{nil,4}}},
        {record_field,5,{atom,5,module},{atom,5,scratch}}]}},
     {attribute,Line,record,
      {context,
       [{record_field,1,{atom,1,s},{nil,1}},
        {record_field,2,{atom,2,r},{nil,2}},
        {record_field,3,{atom,3,cp}},
        {record_field,4,{atom,4,compile},{atom,4,false}},
        {record_field,5,{atom,5,here}},
        {record_field,6,{atom,6,latest}},
        {record_field,7,{atom,7,source},{record,8,src,[]}},
        {record_field,8,{atom,8,debug},{integer,8,0}}]}},
     {function, Line, immed, 1,
      [{clause, Line, [{var, Line, '_'}], [], [{atom, Line, Immediate}]}]}].

default_dispatch_code() ->
    C = gen_var(0),
    {function, 0, 'm dispatch', 1,
     [{clause, 0, [{var, 0, C}], [], [{var, 0, C}]}]}.

retrieve_codes(Module) ->
    {ren_codes, Codes} = lists:keyfind(ren_codes, 1, Module:module_info(attributes)),
    Codes.

update_codes_attribute(Codes) ->
    update_codes_attribute(Codes, Codes, []).
update_codes_attribute(_, [], Acc) ->
    lists:reverse(Acc);
update_codes_attribute(Codes, [{attribute, Line, ren_codes, _}|T], Acc) ->
    update_codes_attribute(Codes, T, [{attribute, Line, ren_codes, Codes}|Acc]);
update_codes_attribute(Codes, [H|T], Acc) ->
    update_codes_attribute(Codes, T, [H|Acc]).

add_function(Codes, Word, Line, Clauses) ->
    Codes2 = lists:map(fun(X) ->
                               case X of
                                   {attribute,_,export,Exports} ->
                                       {attribute, Line, export,
                                        lists:keystore(Word, 1, Exports, {Word,1})};
                                   {function, _, Word, 1, _} ->
                                       {function, Line, Word, 1, Clauses};
                                   It -> It
                               end
                       end, Codes),
    case lists:any(fun(X) ->
                           case X of
                               {function, _, Word, 1, _} -> true;
                               _ -> false
                           end
                   end, Codes2) of
        true -> Codes2;
        _ -> Codes2 ++ [{function, Line, Word, 1, Clauses}]
    end.

add_method(Codes, Line, Clauses) ->
    add_method(Codes, Line, Clauses, []).
add_method([], Line, Clauses, Acc) ->
    lists:reverse([{function, Line, 'm dispatch', 1, Clauses}|Acc]);
add_method([{attribute, Line, export, Export}=H|T], Line2, Clauses, Acc) ->
    case lists:keyfind('m dispatch', 1, Export) of
        false ->
            add_method(T, Line2, Clauses,
                       [{attribute, Line, export, [{'m dispatch', 1}|Export]}|Acc]);
        _ ->
            add_method(T, Line2, Clauses, [H|Acc])
    end;
add_method([{function, Line, 'm dispatch', 1, Existing}|T], _, Adding, Acc) ->
    lists:reverse([{function, Line, 'm dispatch', 1,
                    merge_method_clauses(Existing, lists:reverse(Adding))}|Acc]) ++ T;
add_method([H|T], Line, Clauses, Acc) ->
    add_method(T, Line, Clauses, [H|Acc]).

merge_method_clauses(Existing, []) ->
    Existing;
merge_method_clauses(Existing, [H|T]) ->
    merge_method_clauses(merge_method_clauses_one(lists:reverse(Existing), H, []), T).

merge_method_clauses_one([], Adding, Acc) ->
    [Adding|Acc];
merge_method_clauses_one([H|T], Adding, Acc) ->
    case same_pattern(H, Adding) of
        true ->
            lists:reverse(T) ++ [Adding|Acc];
        false ->
            merge_method_clauses_one(T, Adding, [H|Acc])
    end.

same_pattern({_, _, X, _, _}, {_, _, Y, _, _}) ->
    flatten_pattern(X, []) == flatten_pattern(Y, []).

flatten_pattern([], Acc) ->
    Acc;
flatten_pattern([H|T], Acc) when is_tuple(H) ->
    [Tag,_Line|Rest] = tuple_to_list(H),
    flatten_pattern(T, flatten_pattern(Tag, flatten_pattern(Rest, Acc)));
flatten_pattern([H|T], Acc) when is_list(H) ->
    flatten_pattern(H, flatten_pattern(T, Acc));
flatten_pattern([H|T], Acc) ->
    flatten_pattern(T, [H|Acc]);
flatten_pattern(X, Acc) ->
    [X|Acc].

make_clauses(Codes, Current, Use, C, N) ->
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
    {Clause, Codes2, _} = make_one_clause(Codes1, Current, Use, Acc, C3, N3),
    [{clause, 0,
      Arg,
      [],
      Clause}|case Codes2 of
                  [] -> [];
                  _ -> make_clauses(Codes2, Current, Use, C, N)
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

make_one_clause([], _, _, Acc, C, N) ->
    end_make_one_clause([], Acc, C, N);
make_one_clause([{atom, Line, lit}, Literal|T], Current, Use, Acc, C, N) ->
    C1 = gen_var(N+1),
    make_one_clause(T, Current, Use,
                    [{match, Line,
                      {var, Line, C1},
                      {call, Line,
                       {remote, Line, {atom, Line, ren}, {atom, Line, lit}},
                       [Literal,
                        {var, Line, C}]}} | Acc],
                    C1,
                    N + 1);
make_one_clause([{atom, _, '(('}|_]=T, _, _, Acc, C, N) ->
    end_make_one_clause(T, Acc, C, N);
make_one_clause([{atom, _, '='}|T], Current, Use, Acc, C, N) ->
    {Pattern, T2} = make_pattern(T),
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    make_one_clause(T2, Current, Use,
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
make_one_clause([{atom, Line, 'receive'}|T], Current, Use, Acc, C, N) ->
    {Receive, T2, N2} =
        case make_receive_clauses(T, Current, Use, C, N, []) of
            {ReceiveClauses, T1, N1} ->
                {{'receive', Line, ReceiveClauses}, T1, N1};
            {ReceiveClauses, Milliseconds, AfterClauses, T1, N1} ->
                {{'receive', Line, ReceiveClauses, Milliseconds, AfterClauses}, T1, N1}
        end,
    C3 = gen_var(N2 + 1),
    make_one_clause(T2, Current, Use,
                    [{match, Line, {var, Line, C3}, Receive}|Acc],
                    C3,
                    N2 + 1);
make_one_clause([{atom, Line, 'case'}|T], Current, Use, Acc, C, N) ->
    SH = gen_var(N+1),
    ST = gen_var(N+2),
    C2 = gen_var(N+3),
    {CaseClauses, T2, N2} = make_case_clauses(T, Current, Use, C2, N+3, []),
    C3 = gen_var(N2 + 1),
    make_one_clause(T2, Current, Use,
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
make_one_clause([{atom, Line, 'm dispatch'}|T], Current, Use, Acc, C, N) ->
    C1 = gen_var(N+1),
    make_one_clause(T, Current, Use,
                    [{match, Line,
                      {var, Line, C1},
                      {call, Line, {atom, Line, 'm dispatch'}, [{var, Line, C}]}} | Acc],
                    C1,
                    N + 1);
make_one_clause([{atom, Line, Function}|T], Current, Use, Acc, C, N) ->
    C1 = gen_var(N+1),
    {M, W} = module_word(Function, Current, Use),
    make_one_clause(T, Current, Use,
                    [{match, Line,
                      {var, Line, C1},
                      {call, Line,
                       {remote, Line, {atom, Line, M}, {atom, Line, W}},
                       [{var, Line, C}]}} | Acc],
                    C1,
                    N + 1);
make_one_clause([{block, Line, Block}|T], Current, Use, Acc, C, N) ->
    C1 = gen_var(N+1),
    List = block_to_list(Block, Line),
    make_one_clause(T, Current, Use,
                    [{match, Line,
                      {var, Line, C1},
                      {call, Line,
                       {remote, Line, {atom, Line, ren}, {atom, Line, lit}},
                       [List,
                        {var, Line, C}]}} | Acc],
                    C1,
                    N + 1);
make_one_clause([{erlang, Line, {Module, Function, Arity}}|T], Current, Use, Acc, C, N) ->
    Args = gen_var(N+1),
    Rest = gen_var(N+2),
    C1 = gen_var(N+3),
    make_one_clause(T, Current, Use,
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
make_one_clause([{Type, Line, Value}|T], Current, Use, Acc, C, N) ->
    C1 = gen_var(N+1),
    make_one_clause(T, Current, Use,
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
call_block([H|T], #context{source=#src{module=Current, use=Use}}=C) when is_atom(H) ->
    {M, W} = module_word(H, Current, Use),
    call_block(T, apply(M, W, [C]));
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
eval({atom, Line, Atom}, #context{source=#src{module=Current, use=Use}}=C, B) ->
    B1 = erl_eval:add_binding('# C', C, B),
    {M, W} = module_word(Atom, Current, Use),
    Expr = {call, Line,
            {remote, Line, {atom, Line, M}, {atom, Line, W}},
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


interpret(#context{s=S, r=R, compile=Compile, here=H, debug=Debug,
                   source=#src{module=Current, use=Use}}=C, B) ->
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
    {Word, C2} = word(C),
    try
        case Word of
            {var, _, _}=Var ->
                case Compile of
                    false ->
                        {C3, B3} = eval(Var, C2, B),
                        interpret(C3, B3);
                    true ->
                        interpret(comma(Var, C2), B)
                end;
            {Type, _, N}=Number when Type == integer; Type == float ->
                case Compile of
                    false ->
                        interpret(C2#context{s=[N|S]}, B);
                    true ->
                        interpret(comma(Number, C2), B)
                end;
            {erlang, _, {_Module, _Function, _Arity}}=ErlangFunction ->
                case Compile of
                    true ->
                        interpret(comma(ErlangFunction, C2), B);
                    _ ->
                        {C3, B3} = eval(ErlangFunction, C2, B),
                        interpret(C3, B3)
                end;
            {atom, _, A}=Atom ->
                case {immed(Current, Use, A), Compile} of
                    {false, true} ->
                        interpret(comma(Atom, C2), B);
                    _ ->
                        {C3, B3} = eval(Atom, C2, B),
                        interpret(C3, B3)
                end;
            {quote, Line, Atom} ->
                case Compile of
                    false ->
                        interpret(C2#context{s=[Atom|S]}, B);
                    _ ->
                        interpret(comma({atom, Line, Atom}, comma({atom, Line, lit}, C2)), B)
                end
        end
    catch
        exit:bye ->
            ok;
        _:E ->
            io:format("error: ~p, word: ~p\n~p.\n", [E, Word, erlang:get_stacktrace()]),
            interpret(C2, B)
    end.


i() ->
    interpret(load(#context{s=["ren.fth"]}), erl_eval:new_bindings()).

d() ->
    interpret(load(#context{s=["ren.fth"], debug=1}), erl_eval:new_bindings()).

test() ->
    C1 = load(#context{s=["test.fth"]}),
    C2 = load(C1#context{s=["ren.fth"]}),
    interpret(C2, erl_eval:new_bindings()).
