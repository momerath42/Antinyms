-module(util).

-compile(export_all).


format(FStr,FArgs) ->
    lists:flatten(io_lib:format(FStr,FArgs)).

randFromList(L) ->
    randFromList(L,erlang:length(L)).

randFromList(L,Length) ->
    lists:nth(random:uniform(Length),L).

filterErr(L) ->
    lists:filter(fun(err) -> false; (_) -> true end,L).

shuffle(L) ->
    lists:sort(fun(_,_) ->
		       case random:uniform() of
			   N when N < 0.5 ->
			       true;
			   _ ->
			       false
		       end
	       end, L).

pushNew(A,L) ->
    case lists:member(A,L) of
        true ->
            L;
        false ->
            [A|L]
    end.

listUnion(L1,L2) ->
    lists:foldl(fun(LE,Acc) ->
			pushNew(LE,Acc)
		end,L2,L1).

uniqueFlatten([L1|LR]) ->
    uniqueFlatten([L1|LR],[]).

uniqueFlatten([],R) ->
    R;
uniqueFlatten([L1|LR],R) ->
    uniqueFlatten(LR,listUnion(L1,R)).

numMessagesWaiting() ->
    case process_info(self(),message_queue_len) of
        {message_queue_len,N} ->
            N;
        _ -> exit(wtf)
    end.

deleteAll(O,L) ->
    [ V || V <- L,
           V /= O ].

histogramFromAListVals(AList) ->
    lists:foldl(fun({_,Val}, Histo) ->
			case lists:keysearch(Val, 1, Histo) of
			    {value, {Val, Count}} ->
				lists:keyreplace(Val,1,Histo,{Val,Count + 1});
			    _ ->
				[{Val,1} | Histo]
			end
		end, [], AList).

orEmpty(L) when is_list(L) ->
    L;
orEmpty(undefined) ->
    [].
