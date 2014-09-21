-module(wordnet_to_custom_thing).

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

-define(SYNONYM,40).
-define(HYPERNYM,1).
-define(HYPONYM,2).
-define(ANTONYM,30).

-include("word.hrl").

wordIdOfSynSet(SynId) ->
    %% todo: make sure taking the first one isn't a problem
    [[WordId]|_] = wordnet:squery("select wordid from wordsXsensesXsynsets where synsetid = ~p",[SynId]),
    WordId.

wordsLinkedBy(Word,LinkId) ->
    [ wordIdOfSynSet(SynId) || [SynId] <- wordnet:squery("select synset2id from semlinks, wordsXsensesXsynsets where linkid = ~p and synsetid = synset1id and wordid = ~p",[LinkId, Word]) ].

waitForSetup2(0) ->
    ok;
waitForSetup2(NResponsesExpected) ->
    receive
        %% for ripple-setup
        %% { setup2_started, _WordPID } ->
        %%     waitForSetup2(NResponsesExpected + 1);
        { done_pinging, WordId, _WordPID, _Iterations } ->
            if ((NResponsesExpected rem 1000) == 0) or (NResponsesExpected < 20) ->
                    lager:warning("waitForSetup2 ~p left - got done_pinging from:~p",
                                  [NResponsesExpected,WordId]);
               true -> dn
            end,
            waitForSetup2(NResponsesExpected - 1)
    end.

initAll() ->
    T1 = erlang:now(),
    {ConnectedWordPids,_CCH} = init(),
    T2 = erlang:now(),
    lager:warning("init finished - starting init2 - ~p seconds",[timer:now_diff(T2,T1)/1000000]),
    init2(ConnectedWordPids),
    T3 = erlang:now(),
    lager:warning("init2 finished - starting init3 - ~p seconds",[timer:now_diff(T3,T2)/1000000]),
    init3(ConnectedWordPids),
    T4 = erlang:now(),
    lager:warning("init3 finished - ~p seconds - ~p total",
                  [timer:now_diff(T4,T3)/1000000,
                   timer:now_diff(T4,T1)/1000000]).

initNoPing() ->
    T1 = erlang:now(),
    {WordPids,_CCH} = init(),
    T2 = erlang:now(),
    lager:warning("init finished - starting init2 - ~p seconds",[timer:now_diff(T2,T1)/1000000]),
    init2(WordPids),
    T3 = erlang:now(),
    lager:warning("init2 finished - starting init3 - ~p seconds",[timer:now_diff(T3,T2)/1000000]),
    lager:error("initNoPing sending setup2_done"),
    [ P ! setup2_done || P <- WordPids ],
    T4 = erlang:now(),
    lager:warning("initNoPing finished - ~p seconds - ~p total",
                  [timer:now_diff(T4,T3)/1000000,
                   timer:now_diff(T4,T1)/1000000]),
    {ok,length(WordPids)}.


initTest() ->
    {ConnectedWordPids,_CCH} = init(),
    lager:warning("init finished - starting init2"),
    init2(ConnectedWordPids),
    PersonId = wordnet:wordId("person"),
    PersonPID = global:whereis_name(PersonId),
    lager:warning("init2 finished - testing setup2 on 'person' (~p @~p)",[PersonId,PersonPID]),
    PersonPID ! setup2,
    waitForSetup2(1).
    %% receive
    %%     { done_pinging, PersonId, PersonPID } ->
    %%         lager:warning("person finished pinging!"),
    %%         PersonPID ! setup2_done
    %% end.

init() ->
    lager:warning("init!"),
    Words = [{W,L} || [W,L] <- wordnet:squery("select wordid,lemma from words") ],
    lager:warning("init starting ~p words",[length(Words)]),
    WordPidsAndConnectionCounts = [ connectNyms(WordId,Lemma) || {WordId,Lemma} <- Words ],
    ConnectedWordPidsAndConnectionCounts = util:filterErr(WordPidsAndConnectionCounts),
    ConnectionCountHisto = util:histogramFromAListVals(ConnectedWordPidsAndConnectionCounts),
    lager:warning("connection count histogram:~n~p",[ConnectionCountHisto]),
    WordPids = [ WP || {WP,_} <- ConnectedWordPidsAndConnectionCounts ],
    {WordPids,ConnectionCountHisto}.

initWithSerialHubSetup() ->
    lager:warning("init!"),
    T0 = erlang:now(),
    Words = [{W,L} || [W,L] <- wordnet:squery("select wordid,lemma from words") ],
    lager:warning("init starting ~p words",[length(Words)]),
    WordPidsAndConnectionCounts = [ connectNyms(WordId,Lemma) || {WordId,Lemma} <- Words ],
    T1 = erlang:now(),
    lager:error("initWithSerialHubSetup initial startup took:~p seconds",[timer:now_diff(T1,T0)/1000000]),
    ConnectedWordPidsAndConnectionCounts = util:filterErr(WordPidsAndConnectionCounts),
    WordPids = [ WP || {WP,_} <- ConnectedWordPidsAndConnectionCounts ],
    %init2(WordPids),
    T2 = erlang:now(),
    %% lager:error("initWithSerialHubSetup setup1 took:~p seconds",[timer:now_diff(T2,T1)/1000000]),
    ConnectionCountHisto = util:histogramFromAListVals(ConnectedWordPidsAndConnectionCounts),
    lager:warning("connection count histogram:~n~p",[ConnectionCountHisto]),
    Setup = fun(WP) ->
                    lager:warning("initWithSerialHubSetup sending setup2 to ~p",[WP]),
                    WP ! setup2,
                    waitForSetup2(1),
                    1
            end,
    [ Setup(WP) || {WP,CC} <- ConnectedWordPidsAndConnectionCounts,
                   CC >= ?HUB_CONNECTIONS_THRESHOLD ],
    T3 = erlang:now(),
    lager:error("initWithSerialHubSetup setup2 of hubs took:~p seconds",[timer:now_diff(T3,T2)/1000000]),
    timer:sleep(30000),
    [ P ! setup2_done || P <- WordPids ],
    T4 = erlang:now(),
    lager:warning("initWithSerialHubSetup finished setup - total: ~p seconds - current mem usage:~p",[timer:now_diff(T4,T0)/1000000,performance:report()]),
    timer:sleep(30000),
    serializeAll(WordPids),
    T5 = erlang:now(),
    lager:warning("initWithSerialHubSetup finished serialization in ~p seconds",[timer:now_diff(T5,T4)/1000000]),
    {WordPids,ConnectionCountHisto}.

serializeAll([]) ->
    ok;
serializeAll([W|R]) ->
    Ref = make_ref(),
    timer:sleep(500),
    W ! {serialize, Ref, self()},
    receive
        { serialized, _WordId, Ref } ->
            serializeAll(R)
    after 90000 ->
            lager:error("serializeAll ~p took too long - retrying",[W]),
            serializeAll([W|R])
    end.

init2(WordPids) ->
    F = fun(P) ->
                P ! setup1
%                erlang:yield()
        end,
    [ F(P) || P <- WordPids ],
    WordPids.

init3_sync(WordPids) ->
    lager:error("init3_sync word-count:~p~n",[length(WordPids)]),
    F = fun(P) ->
                P ! setup2,
                receive
                    { done_pinging, _W, P, _Iterations } ->
                        ok
                end
        end,
    [ F(P) || P <- WordPids ],
%    waitForSetup2(length(WordPids)),
    [ P ! setup2_done || P <- WordPids ],
    {ok,length(WordPids)}.

init3(WordPids) ->%Ordered) ->
%    TestWordsPids = lists:sublist(WordPids,10),
%    WordPids = util:shuffle(WordPidsOrdered),
    lager:error("init3 shuffled pids - count:~p~n",[length(WordPids)]),
    F = fun(P) ->
                P ! setup2,
                timer:sleep(5)
        end,
    [ F(P) || P <- WordPids ],
    lager:error("init3 finished sending setup2"),
    waitForSetup2(length(WordPids)),
    lager:error("init3 sending setup2_done"),
    [ P ! setup2_done || P <- WordPids ],
    {ok,length(WordPids)}.

connectNyms(WordId,Lemma) ->
    connectNyms(WordId,Lemma,false).

connectNyms(WordId,Lemma,Kludge) ->
    Syn = wordsLinkedBy(WordId,?SYNONYM),
    Hyper = wordsLinkedBy(WordId,?HYPERNYM),
    Hypo = wordsLinkedBy(WordId,?HYPONYM),
    Anto = wordsLinkedBy(WordId,?ANTONYM),
    NConnections = length(Syn)+length(Hyper)+length(Hypo)+length(Anto),
    if (NConnections > 0) or Kludge ->
            WordPID = word:start(WordId,Lemma,Anto,Syn,Hyper,Hypo),
            %%lager:warning("connectNyms(~p,~p) started word:~p with ~p connections",[WordId,Lemma,WordPID,NConnections]),
            {WordPID,NConnections};
       true ->
            %lager:warning("connectNyms(~p,~p) discarding isolated word",[WordId,Lemma]),
            err
    end.
