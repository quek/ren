-module(rensa).
-compile(export_all).
-include("rensa.hrl").

next(#context{cp=[X|XS], s=S, r=R, here=H}=C) ->
    io:format("next: s~p r~p cp~p here~p\n", [S, R, [X|XS], H]),
    apply(X, [C#context{cp=XS}]);
next(C) ->
    io:format("!!!next: ~p\n", [C]).

do_list(#context{s=[X|XS], cp=CP, r=R}=C) ->
    next(C#context{s=XS, cp=X, r=[CP|R]}).

make_fun(Codes, C) ->
    make_fun(Codes, [], C).

make_fun([], Compiled, _) ->
    fun(#context{s=S}=C) ->
            do_list(C#context{s=[lists:reverse(Compiled)|S]})
    end;
make_fun([X|XS], Compiled, C) ->
    case find(X, C) of
        error ->
            case to_number(X) of
                {ok, N} ->
                    make_fun(XS, [N|Compiled], C);
                _ ->
                    io:format("error ~p!", [X]),
                    exit(C)
            end;
        #word{f=F} ->
            make_fun(XS, [F|Compiled], C)
    end.




find(Word, #context{d=D}) ->
    case maps:find(Word, D) of
        {ok, #word{hidden=false}=X} ->
            X;
        _ ->
            error
    end.



init_context(C) ->
    lists:foldl(fun({Name, Codes, Immed}, #context{d=D}=Cn) ->
                        Cn#context{
                          d = maps:put(Name,
                                       #word{
                                          f = make_fun(Codes, Cn),
                                          immed = Immed
                                         },
                                       D)
                         }
                end,
                C,
                [{"1+", ["lit", "1", "+", "exit"], false}
                ,{"1-", ["lit", "1", "-", "exit"], false}
                ,{":", ["word",
                        "header",
                        "latest", "hidden",
                        "]",
                        "exit"], false}
                ]).

main() ->
    Exit = fun(#context{r=[X|XS]}=C) ->
                   next(C#context{r=XS, cp=X})
           end,
    C = #context{
           d = #{
             "[" =>
                 #word{
                    f = fun(C) ->
                                next(C#context{compile=false})
                        end
                   },
             "]" =>
                 #word{
                    f = fun(C) ->
                                next(C#context{compile=true})
                        end
                   },
             "latest" =>
                 #word{
                    f = fun(#context{s=S, latest=L}=C) ->
                                next(C#context{s=[L|S]})
                        end
                   },
             "hidden" =>
                 #word{
                    f = fun(#context{s=[S|SS], d=D}=C) ->
                                W = maps:get(S, D),
                                W2 = W#word{hidden=true},
                                next(C#context{s=SS, d=maps:put(S, W2, D)})
                        end
                   },
             "comma" =>
                 #word{f = comma},
             "exit" =>
                 #word{
                    name = "exit",
                    f = Exit
                   },
             "lit" =>
                 #word{
                    name = "lit",
                    f = fun(#context{cp=[X|XS], s=S}=C) ->
                                next(C#context{cp=XS, s=[X|S]})
                        end
                   },
             "word" =>
                 #word{
                    name = "word",
                    f = fun(#context{s=S}=C) ->
                                {Word, C2} = word(C),
                                next(C2#context{s=[Word|S]})
                        end
                   },
             "header" =>
                 #word{
                    name = "header",
                    f = fun(#context{s=[Word|S], d=D}=C) ->
                                next(C#context{
                                       s = S,
                                       d = maps:put(Word,
                                                    #word{
                                                       hidden = true
                                                      },
                                                    D),
                                       latest = Word,
                                       here = []
                                      })
                        end
                   },
             ";" =>
                 #word{
                    name = ";",
                    f = fun(#context{d=D, here=H, latest=Word}=C) ->
                                next(C#context{
                                       d = maps:put(Word,
                                                    #word{
                                                       name = Word,
                                                       f = make_fun([], [Exit|H], C)
                                                      },
                                                    D),
                                       here = [],
                                       compile = false
                                      })
                        end,
                    immed=true
                   },
             "," =>
                 #word{
                    name = ",",
                    f = fun(#context{s=[S|SS], here=H}=C) ->
                                next(C#context{s=SS, here=[S|H]})
                        end
                   },
             "+" =>
                 #word{
                    name = "+",
                    f = fun(#context{s=[A,B|X]}=C) ->
                                next(C#context{s=[A+B|X]})
                        end
                   },
             "-" =>
                 #word{
                    name= "-",
                    f = fun(#context{s=[A,B|X]}=C) ->
                                next(C#context{s=[B-A|X]})
                        end
                   },
             "p" =>
                 #word{
                    f = fun(#context{s=[X|XS]}=C) ->
                                io:format("~p\n", [X]),
                                next(C#context{s=XS})
                        end
                   },
             ".s" =>
                 #word{
                    f = fun(#context{s=S}=C) ->
                                io:format("~p\n", [S]),
                                next(C)
                        end
                   },
             "quit" =>
                 #word{ f = fun(_) -> exit("quit") end },
             "interpret" =>
                 #word {
                    f = fun(#context{d=D}=C) ->
                                #word{f=F} = maps:get("interpret", D),
                                next(interpret(C#context{cp=[F]}))
                        end
                   }
            }
          },
    #context{d=D}=C2 = init_context(C),
    #word{f=F} = maps:get("interpret", D),
    next(C2#context{cp=[F]}).

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


interpret(#context{s=S, compile=Compile}=C) ->
    {Word, C2} = word(C),
    case find(Word, C) of
        error ->
            case to_number(Word) of
                {ok, N} ->
                    case Compile of
                        false ->
                            C2#context{s=[N|S]};
                        true ->
                            comma(fun(#context{s=X}=CC) ->
                                          CC#context{s=[N|X]}
                                  end,
                                  C2)
                    end;
                _ ->
                    io:format("~s is unknow.\n", [Word]),
                    C2
            end;
        #word{f=F, immed=Immed} ->
            case {Immed, Compile} of
                {false, true} ->
                    comma(F, C2);
                _ ->
                    apply(F, [C2])
            end
    end.
