-module(ren).
-compile(export_all).
-include("ren.hrl").

immed(_) ->
    false.

xfind(Word, #context{d=D}) ->
    case maps:find(Word, D) of
        {ok, #word{hidden=false}=X} ->
            X;
        _ ->
            error
    end.

find(Word, _) ->
    try
        A = list_to_existing_atom(Word),
        case erlang:function_exported(ren, A, 1) of
            true -> A;
            _ -> error
        end
    catch error:badarg ->
            error
    end.

key(#context{buffer=[X|XS]}=C) ->
    {X, C#context{buffer=XS}};
key(C) ->
    [X|XS] = io:get_line(""),
    {X, C#context{buffer=XS}}.

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

comma(F, #context{here=H}=C) ->
    C#context{here=[F|H]}.

dup(#context{s=[H|T]}=C) ->
    C#context{s=[H,H|T]}.

quit(_) ->
    exit("quit").

interpret(#context{s=S, r=R, compile=Compile}=C) ->
    io:format("next: s~p r~p\n", [S, R]),
    {Word, C2} = word(C),
    case find(Word, C) of
        error ->
            case to_number(Word) of
                {ok, N} ->
                    case Compile of
                        false ->
                            interpret(C2#context{s=[N|S]});
                        true ->
                            interpret(comma(fun(#context{s=X}=CC) ->
                                                    CC#context{s=[N|X]}
                                            end,
                                            C2))
                    end;
                _ ->
                    io:format("~s is unknow.\n", [Word]),
                    interpret(C2)
            end;
        A ->
            case {immed(A), Compile} of
                {false, true} ->
                    interpret(comma(A, C2));
                _ ->
                    interpret(apply(ren, A, [C2]))
            end
    end.

interpret() ->
    C = #context{},
    interpret(C).
