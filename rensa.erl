-module(rensa).
-compile(export_all).
-include("rensa.hrl").

next(#rensa_context{cp=[X|XS], s=S, r=R}=C) ->
    io:format("next: ~p ~p\n", [S, R]),
    apply(X, [C#rensa_context{cp=XS}]);
next(C) ->
    io:format("!!!next: ~p\n", [C]).

do_list(#rensa_context{s=[X|XS], cp=CP, r=R}=C) ->
    next(C#rensa_context{s=XS, cp=X, r=[CP|R]}).

make_fun(Codes, C) ->
    make_fun(Codes, [], C).

make_fun([], Compiled, _) ->
    fun(#rensa_context{s=S}=C) ->
            do_list(C#rensa_context{s=[lists:reverse(Compiled)|S]})
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
        F ->
            make_fun(XS, [F|Compiled], C)
    end.




find(Word, #rensa_context{d=D}) ->
    case maps:find(Word, D) of
        {ok, #rensa_word{f=F, hidden=false}} ->
            F;
        _ ->
            error
    end.



init_context(C) ->
    lists:foldl(fun({Name, Codes, Immed}, #rensa_context{d=D}=Cn) ->
                        Cn#rensa_context{
                          d = maps:put(Name,
                                       #rensa_word{
                                          f = make_fun(Codes, Cn),
                                          immed = Immed
                                         },
                                       D)
                         }
                end,
                C,
                [{"1+", ["lit", "1", "+", "exit"], false}
                ,{"1-", ["lit", "1", "-", "exit"], false}
                %,{":", ["word",
                %        "header",
                %        "lit",
                %        "docol_code",
                %        "comma",
                %        "latest", "fetch", "hidden",
                %        "rbrac",
                %        "exit"], false}
                ]).

main() ->
    C = #rensa_context{
           d = #{
             "exit" =>
                 #rensa_word{
                    name = "exit",
                    f = fun(#rensa_context{r=[X|XS], cp=CP}=C) ->
                                next(C#rensa_context{r=XS, cp=[X|CP]})
                        end
                   },
             "lit" =>
                 #rensa_word{
                    name = "lit",
                    f = fun(#rensa_context{cp=[X|XS], s=S}=C) ->
                                next(C#rensa_context{cp=XS, s=[X|S]})
                        end
                   },
             "word" =>
                 #rensa_word{
                    name = "word",
                    f = fun(#rensa_context{s=S}=C) ->
                                {Word, C2} = word(C),
                                next(C2#rensa_context{s=[Word|S]})
                        end
                   },
             "header" =>
                 #rensa_word{
                    name = "header",
                    f = fun(#rensa_context{s=[Word|S], d=D}=C) ->
                                next(C#rensa_context{
                                       s = S,
                                       d = maps:put(Word,
                                                    #rensa_word{
                                                       hidden = true
                                                      },
                                                    D),
                                       latest = Word,
                                       here = []
                                      })
                        end
                   },
             ";" =>
                 #rensa_word{
                    name = ";",
                    f = fun(#rensa_context{d=D, here=H, latest=Word}=C) ->
                                next(C#rensa_context{
                                       d = maps:put(Word,
                                                    #rensa_word{
                                                       name = Word,
                                                       f = make_fun(lists:reverse(H), C)
                                                      },
                                                    D),
                                       here = [],
                                       compile = false
                                      })
                        end
                   },
             "," =>
                 #rensa_word{
                    name = ",",
                    f = fun(#rensa_context{s=[S|SS], here=H}=C) ->
                                next(C#rensa_context{s=SS, here=[S|H]})
                        end
                   },
             "+" =>
                 #rensa_word{
                    name = "+",
                    f = fun(#rensa_context{s=[A,B|X]}=C) ->
                                next(C#rensa_context{s=[A+B|X]})
                        end
                   },
             "-" =>
                 #rensa_word{
                    name= "-",
                    f = fun(#rensa_context{s=[A,B|X]}=C) ->
                                next(C#rensa_context{s=[B-A|X]})
                        end
                   },
             "p" =>
                 #rensa_word{
                    f = fun(#rensa_context{s=[X|XS]}=C) ->
                                io:format("~p\n", [X]),
                                next(C#rensa_context{s=XS})
                        end
                   },
             ".s" =>
                 #rensa_word{
                    f = fun(#rensa_context{s=S}=C) ->
                                io:format("~p\n", [S]),
                                next(C)
                        end
                   },
             "quit" =>
                 #rensa_word{ f = fun(_) -> exit("quit") end },
             "interpret" =>
                 #rensa_word {
                    f = fun(#rensa_context{d=D}=C) ->
                                #rensa_word{f=F} = maps:get("interpret", D),
                                next(interpret(C#rensa_context{cp=[F]}))
                        end
                   }
            }
          },
    #rensa_context{d=D}=C2 = init_context(C),
    #rensa_word{f=F} = maps:get("interpret", D),
    next(C2#rensa_context{cp=[F]}).

key(#rensa_context{buffer=[X|XS]}=C) ->
    {X, C#rensa_context{buffer=XS}};
key(C) ->
    [X|XS] = io:get_line(""),
    {X, C#rensa_context{buffer=XS}}.

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

comma(F, #rensa_context{here=H}=C) ->
    C#rensa_context{here=[F|H]}.


interpret(#rensa_context{s=S, compile=Compile}=C) ->
    {Word, C2} = word(C),
    case find(Word, C) of
        error ->
            case to_number(Word) of
                {ok, N} ->
                    case Compile of
                        false ->
                            C2#rensa_context{s=[N|S]};
                        true ->
                            comma(fun(#rensa_context{s=X}=CC) ->
                                          CC#rensa_context{s=[N|X]}
                                  end,
                                  C2)
                    end;
                _ ->
                    io:format("~s is unknow.\n", [Word]),
                    C2
            end;
        F ->
            case Compile of
                false ->
                    apply(F, [C2]);
                true ->
                    comma(F, C2)
            end
    end.
