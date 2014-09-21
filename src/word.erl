% WORD ORiented Database
% (c) Michael Warnock 2010, 2014
% 2-clause BSD licensed (see /LICENSE.md)

%% An experimental, single-purpose, distributable graph-database, with a
%% ridiculously long setup time (pingAndBePinged), and assumptions of
%% static structure (mostly in that post-setup (dis)connections aren't
%% implemented).

%% It is single purpose (edge-types/predicates enumerated and flattened;
%% domain-specific naming; not encapsulated as a library) because I find
%% it most productive to experiment in a concrete domain without
%% worrying about generalization; in theory, this will be useful for
%% other small-world graphs, if it proves useful for this one.

%% Strategy:

%% 1. Find a "hub threshold" for number of connnections (could be done
%% algorithmically, but so far guessed-at based on
%% 'nym'-connection-count distribution and defined as
%% HUB_CONNECTIONS_THRESHOLD).

%% 1.5 TODO: Prune redundant hubs.  Define a fitness function for a node,
%% taking into account how many non-hub nodes reference a hub, and how
%% many other hubs a given hub can see.

%% 2. When a ping hits a node above that threshold, a pong with 0
%% forwards is returned; the paths that can be inferred are saved and
%% that node begins its own ping wave (ripple-setup; now disabled in
%% favor of a serial setup2 phase).

%% 3. TODO: Make the graph serializable, so setup can happen once,
%% allowing for heavy optimization in the setup phases, for run-time
%% performance or memory-use, at the cost of longer setup times.  Seed a
%% torrent of the serialized form. db-blobs, per-node files, mmap?

%% 4. TODO: When the serialization is available and trusted, allow for a
%% lazy-startup, so memory use can be more reasonable when only a small
%% part of the graph is searched.

%% Hypothesized Consequences:

%% 1. After setup, shortest path search can hit:
%%% a. just one process when the origin or destination is a hub max-hops
%%% away from the other, or when origin and destination are otherwise
%%% adjacent

%%% b. a max-hops bounded number of processes when origin and
%%% destination are non-hubs within the max-hops bound (non-hub nodes
%%% have a next-hop cache for other non-hub nodes in max-hop range).

%%% c. at most all the hubs in the connected graph, (with optional
%%% (potentially parallel) search orders: BF, DF, IDDFS).

%%%% Note: a naive implementation will not always find a shortest path
%%%% when origin and destination are both non-hub nodes (or even if they
%%%% are hubs, depending on max-dist vs the graph properties, but I
%%%% think I can avoid that).  I have vague notions for how to correct
%%%% for this efficiently (in a third setup pass), but at worst
%%%% connectivity and an upper bound on distance (quite possibly greater
%%%% than max-distance) can be established for use in a traditional
%%%% search.  Many of the applications I have in mind don't necessarily
%%%% care that paths are optimally short.  TODO: There's probably a
%%%% formula for how far from optimal they might be, given the setup
%%%% parameters.

%% 2. Given an optimally chosen threshold (maybe optimum for the
%% available RAM), the performance, and in particular the performance
%% under concurrent load, will beat traditional graph databases with one
%% or two list or matrix data structures per relationship, and be easier
%% to distribute across machines (though clearly a naive distribution of
%% nodes would hurt performance significantly compared to something that
%% tried to partition the graph to minimize links between machines)

%% Note: if the ping-forwarding explosion stops at hubs, the
%% max-distance can hopefully be greater than the distance from any
%% connected node to its closest hub, without memory-blowout. TODO:
%% verify this experimentally (with another graphdb as well).

%% Note: in many respects, any advantage this design has is in caching,
%% and caching is rarely done appropriately without proper run-time
%% analysis.  The choices I've made might be appropriate for the
%% antinyms/wordnet range of workloads, at some level of concurrent
%% (stateful) activity on the graph, or for some other problem/graph, or
%% it might be a total bust.  It's an experiment.

%% Note: I'm not at all sure nodes should be equivalent to wordnet
%% word-ids (which are 1-1 with 'lemmas', ie the sequence of letters),
%% rather than 'word-senses' or some other distinction; it would seem to
%% depend on the particulars of the game/toy/application, so I lean
%% toward a situation that creates larger (worst-case /
%% more-interesting) graphs.  Likewise, many WordNet 'words',
%% disproportionately represented among hubs (see '* genus'), are not
%% single words.  Excluding them (or any subset) comes with its own
%% problems, and (I believe prematurely) spreads-out and/or breaks-up
%% the graph.

-module(word).

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

-include("word.hrl").

%% crude limits on setup2 ping-explosion; intended to become obsolete
-define(MAX_PING_HOPS,6).
-define(HOP_DELAY_POWER,1.7).
-define(ADDITIONAL_RAND_DELAY_RANGE,2000).

start(WordId,Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms) ->
    PID = spawn(?MODULE,init,[WordId,Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,self()]),
    case global:register_name(WordId,PID) of
        no ->
            %% was created by another node a millisecond or so ago
            global:whereis_name(WordId);
        yes ->
            PID
    end.

startFromDisk(WordId) ->
    PID = spawn(fun() ->
                        loadFromDisk(WordId)
                end),
    case global:register_name(WordId,PID) of
        no ->
            %% was created by another node a millisecond or so ago
            global:whereis_name(WordId);
        yes ->
            PID
    end.


init(WordId,Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,SetupManagerPID) ->
    put(lemma,Lemma),
    put(word,WordId),
    {PartitionSize,MaxHopsToAnywhere,OtherNyms} = setup(WordId,Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,SetupManagerPID),
%    io:format("init(~p,~p... finished setup ~n",[WordId,Lemma]),
    GameState = #{occupiers => []},
%    loop(WordId,Lemma,OtherNyms,Antonyms,Synonyms,Hypernyms,Hyponyms,PartitionSize,MaxHopsToAnywhere,GameState),
    loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState),
    io:format("init(~p,~p... loop returned! exiting~n",[WordId,Lemma]).
%loop(WordId,Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,PartitionSize,MaxHopsToAnywhere,GameState).

setup(WordId,_Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,SetupManagerPID) ->
    SetupManagerPID ! { ready_for_setup, WordId, self() },
    Nyms = util:uniqueFlatten([Antonyms,Synonyms,Hypernyms,Hyponyms]),
    OtherNyms = case lists:member(WordId,Nyms) of
                    true ->
                        IsAntonym = lists:member(WordId,Antonyms),
                        IsSynonym = lists:member(WordId,Synonyms),
                        IsHypernym = lists:member(WordId,Hypernyms),
                        IsHyponym = lists:member(WordId,Hyponyms),
                        lager:debug("setup(~p,~p... is its own nym! anto:~p syno:~p hyper:~p hypo:~p",
                                   [WordId,_Lemma,IsAntonym,IsSynonym,IsHypernym,IsHyponym]),
                        util:deleteAll(WordId,Nyms);
                    false -> Nyms
                end,
    NOtherNyms = length(OtherNyms),
    IsHub = if NOtherNyms >= ?HUB_CONNECTIONS_THRESHOLD ->
                    lager:warning("setup(~p,~p is-hub at ~p connections!",[WordId,_Lemma,NOtherNyms]),
                    true;
               true -> false
            end,
    %% receive
    %%     setup1 ->
    %%         %% lager:warning("word:setup(~p,~p,~p,~p,~p,~p,~p) received setup1!",
    %%         %%               [WordId,_Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,SetupManagerPID]),
    %%         setup1(OtherNyms)
    %% end,
    %LiveNyms = util:filterErr(NymResults),
    {TerritorySize,MaxDistance,Iterations} = pingAndBePinged(WordId,IsHub,OtherNyms,SetupManagerPID,0,0,0,0),
    lager:debug("word:setup(~p,~p,~p,~p,~p,~p,~p) finished pinging! territory-size:~p max-dist:~p pabp-iterations:~p",
                  [WordId,_Lemma,Antonyms,Synonyms,Hypernyms,Hyponyms,SetupManagerPID,
                  TerritorySize,MaxDistance,Iterations]),
    {TerritorySize,MaxDistance,OtherNyms}.


%% TODO: link and invalidate, to allow for migration between nodes and hibernate-to-disk
pidOf_old(WordId) ->
    case get(WordId) of
        P when is_pid(P) ->
            P;
        _ ->
            lager:error("~p pidOf(~p) failed - this word:~p (~p)",
                        [self(),WordId,get(word),get(lemma)]),
            exit({pidOf,WordId})
    end.

pidOf(WordId) ->
    case global:whereis_name(WordId) of
        P when is_pid(P) ->
            P;
        _ ->
            startFromDisk(WordId)
    end.

lists_intersect(L1,L2) ->
    lists:any(fun(A) ->
                      lists:member(A,L2)
              end,L1).

forwardPings(Nyms,PingerWordId,Hops,Path,PingerPID) ->
    [ forwardPing(W,PingerWordId,Hops,Path,PingerPID)
      || W <- Nyms,
         not lists:member(W,Path),
         W /= PingerWordId ].

forwardPing(NextWordId,PingerWordId,Hops,Path,PingerPID) ->
    PID = pidOf(NextWordId),
    lager:debug("~p ~p forwardPing(~p,~p,~p,~p,~p)",
               [self(),get(lemma),NextWordId,PingerWordId,Hops,Path,PingerPID]),
%    PID ! { ping, PingerWordId, Hops, Path, PingerPID },
%% the longer the path, the more it's probably worth delaying - wish I
%% could think of a way of not having to send more than one pong (maybe
%% something wordnet-graph specific once I know more about its
%% properties)
    timer:send_after(trunc(math:pow(Hops,?HOP_DELAY_POWER))
                     + random:uniform(?ADDITIONAL_RAND_DELAY_RANGE),
                     PID,{ ping, PingerWordId, Hops, Path, PingerPID }),
    NextWordId.

saveHub(_TargetNode,false) -> dn;
saveHub(TargetNode,true) ->
    put(hubs,util:pushNew(TargetNode,util:orEmpty(get(hubs)))).

saveRouteToHub(HubId,Distance,[]) ->
    saveRouteToHub(HubId,Distance,[HubId]);
saveRouteToHub(HubId,Distance,Path) ->
    LastStep = lists:last(Path),
    saveHub(HubId,true),
    saveRoutingInfo(HubId,Distance,LastStep).

%saveRoutingInfo(TargetNode,Distance,[FirstHop|_]=Path) ->
saveRoutingInfo(TargetNode,Distance,FirstHop) ->
    case get({distance,TargetNode}) of
        undefined ->
            lager:debug("saveRoutingInfo(~p,~p,~p) saving",[TargetNode,Distance,FirstHop]),
            put({distance,TargetNode},Distance),
            put({next_hop,TargetNode},FirstHop);
%            put({path,TargetNode},Path),
%            put({n_routes,TargetNode},1);
        CurrentDistance ->
            if Distance < CurrentDistance ->
                    lager:debug("saveRoutingInfo(~p,~p,~p) shorter than existing:~p~n",
                                  [TargetNode,Distance,FirstHop,CurrentDistance]),
                    put({distance,TargetNode},Distance),
                    put({next_hop,TargetNode},FirstHop);
%                    put({path,TargetNode},Path),
%                    put({n_routes,TargetNode},get({n_routes,TargetNode})+1); % non-looping routes
               true ->
                    lager:debug("saveRoutingInfo(~p,~p,~p) longer than existing:~p~n",
                                [TargetNode,Distance,FirstHop,CurrentDistance])
            end
    end.

handlePing(WordId,IsHub,Nyms,PingerWordId,HopsToMe,PathToMe,PingerPID) ->
    case lists:member(WordId,PathToMe) of
        true ->
            lager:warning("~p ~p handlePing(~p,Nyms,~p,~p,~p,~p) self-in-path - shouldn't happen! readable-path:~p",
                          [self(),get(lemma),
                           WordId,
                           PingerWordId,HopsToMe,
                           PathToMe,PingerPID,
                           wordnet:lemmas(PathToMe)]);
        false ->
            NewPath = PathToMe++[WordId],
            NewHops = HopsToMe + 1,
            NForwardedTo = if (NewHops < ?MAX_PING_HOPS) and (not IsHub) ->
                                   ForwardedTo = forwardPings(Nyms,PingerWordId,
                                                              NewHops,NewPath,PingerPID),
                                   length(ForwardedTo);
                              true -> 0
                           end,
            %% SendPong = case get({ponged,PingerPID}) of
            %%                undefined -> put({ponged,PingerPID},HopsToMe), true;
            %%                PreviousPongHops when HopsToMe < PreviousPongHops ->
            %%                    put({ponged,PingerPID},HopsToMe), true;
            %%                _ -> false
            %%            end,
            %% if SendPong ->
                    lager:debug("~p ~p handlePing(~p,Nyms,~p,~p,~p,~p) n-forwarded:~p sending pong",
                                [self(),get(lemma),
                                 WordId,
                                 PingerWordId,HopsToMe,
                                 PathToMe,PingerPID,
                                 NForwardedTo]),
                    %% todo? if recording next-hops and distances works
                    %% in one node, all the connected nodes could save
                    %% what they need from reversing paths and looking
                    %% at all the subpaths
            NForwardedTo
            %%    true -> dn
            %% end
    end.

setup1(Nyms) ->
    [ cachePidAndEnsureExistence(W) || W <- Nyms ].

setup2(WordId,Nyms,_SetupManagerPID) ->
    case get(setup2_started) of
        true ->
            lager:warning("~p setup2(~p,_Nyms,_SetupManagerPID) (~p) not meant to be called twice!",
                          [self(),WordId,get(lemma)]),
            0;
        undefined ->
            put(setup2_started,true),
%% part of ripple-setup:
%            SetupManagerPID ! { setup2_started, self() },
            SendPing = fun(W) ->
                               P = pidOf(W),
                               lager:debug("~p pingAndBePinged(~p... (~p) pinging ~p @~p",
                                           [self(),WordId,get(lemma),W,P]),
                               P ! { ping, WordId, 1, [], self() },
                               P
                       end,
            Sent = [ SendPing(W) || W <- Nyms ],
            length(util:filterErr(Sent))
    end.

%%% TODO: Big Ugly Function
%% The messages have to be received on the same level because it's
%% assumed that connected nodes may be doing this setup process
%% concurrently (maybe it's not worth the performance gains (if any), I
%% don't know: this is an experiment in the performance characteristics
%% of mapping nodes to processes, as well as the expressibility of
%% agent-calculations operating on the graph)
pingAndBePinged(WordId,IsHub,Nyms,SetupManagerPID,NExpectedReplies,NReceivedReplies,MaxDistance,Iterations) ->
%%    lager:warning("~p pingAndBePinged(~p... (get-word:~p lemma:~p)",[self(),WordId,get(word),get(lemma)]),
    receive
        { ping, PingerWordId, HopsToMe, PathToMe, PingerPID } ->
%            PingerPID ! { pong, WordId, HopsToMe, PathToMe, NForwardedTo },
            FirstStepOnPathToMe = case PathToMe of
                                      [FirstStep|_] -> FirstStep;
                                      [] -> WordId % adjacent
                                  end,
            {NPingsInitiated,NPingsForwarded} =
                if IsHub ->
% I like the idea of a ripple-setup, but in the interests of keeping
% memory usage under control, and easier debugging, setup2 is now
% initiated serially by the SetupManager
% (wordnet_to_custom_thing:initWithSerialHubSetup)

%                        NInitiated = setup2(WordId,Nyms,SetupManagerPID),
%                        {NInitiated,0};
                        {0,0};
                   true ->
                        NForwardedTo = handlePing(WordId,IsHub,Nyms,PingerWordId,HopsToMe,PathToMe,PingerPID),
                        saveRouteToHub(PingerWordId,HopsToMe,PathToMe),
                        {0,NForwardedTo}
                end,
            timer:send_after(trunc(math:pow(HopsToMe,?HOP_DELAY_POWER))
                             + random:uniform(?ADDITIONAL_RAND_DELAY_RANGE),
                             PingerPID,{ pong, WordId, HopsToMe, FirstStepOnPathToMe, NPingsForwarded, IsHub }),
            pingAndBePinged(WordId,IsHub,Nyms,SetupManagerPID,NPingsInitiated+NExpectedReplies,NReceivedReplies,MaxDistance,Iterations+1);
%        { pong, Ponger, Distance, PathToPonger, NForwardedPings } ->
        { pong, Ponger, Distance, FirstStepOnPathToPonger, NForwardedPings, PongerIsHub } ->
            saveRoutingInfo(Ponger,Distance,FirstStepOnPathToPonger),
            saveHub(Ponger,PongerIsHub),
            NewMaxDistance = max(MaxDistance,Distance),
            NewNExpectedReplies = NExpectedReplies + NForwardedPings,
            NewNReceivedReplies = NReceivedReplies + 1,
            NMessagesWaiting = util:numMessagesWaiting(),
            if NMessagesWaiting > 3000000 ->
                    process_flag(priority,high);
               true ->
                    process_flag(priority,normal)
            end,
            %% if (NExpectedReplies =< (NReceivedReplies + 10))
            %%    or ((NReceivedReplies rem 100) == 0) ->
            %%         lager:info("~p pingAndBePinged(~p... n-expected:~p n-rec:~p got pong from:~p dist:~p n-forwarded:~p iterations:~p n-msges-waiting:~p",
            %%                    [self(),WordId,NExpectedReplies,NReceivedReplies,
            %%                     Ponger,Distance,NForwardedPings,Iterations,NMessagesWaiting]);
            %%    true ->
                    lager:debug("~p pingAndBePinged(~p... n-expected:~p n-rec:~p got pong from:~p dist:~p n-forwarded:~p iterations:~p n-msges-waiting:~p",
                               [self(),WordId,NExpectedReplies,NReceivedReplies,
                                Ponger,Distance,NForwardedPings,Iterations,NMessagesWaiting]),
            %% end,
            if (NewNExpectedReplies == NewNReceivedReplies)
               and (NMessagesWaiting == 0) ->
                    lager:info("  done_pinging!"),
                    case get(done_pinging) of
                        undefined ->
                            SetupManagerPID ! { done_pinging, WordId, self(), Iterations },
                            put(done_pinging,1);
                        N when is_integer(N) ->
                            lager:warning("word:~p (~p) is done_pinging for the ~p time",
                                          [WordId,get(lemma),N+1]),
                            put(done_pinging,N+1)
                    end;
               true -> still_working
            end,
            pingAndBePinged(WordId,IsHub,Nyms,SetupManagerPID,
                            NewNExpectedReplies,
                            NewNReceivedReplies,
                            NewMaxDistance,Iterations+1);
        setup2 ->
            lager:warning("~p word:~p (~p) got setup2 message",
                          [self(),WordId,get(lemma)]),
            NReallySent = setup2(WordId,Nyms,SetupManagerPID),
            pingAndBePinged(WordId,IsHub,Nyms,SetupManagerPID,NReallySent,NReceivedReplies,MaxDistance,Iterations+1);
        setup2_done ->
            case get(hubs) of
                undefined ->
                    lager:warning("~p pingAndBePinged(~p... (~p) got setup2_done - no hubs (too far, or not connected to any?)",
                                  [self(),WordId,get(lemma)]);
                Hubs ->
                    lager:info("~p pingAndBePinged(~p... (~p) got setup2_done - hubs:~p",
                               [self(),WordId,get(lemma),Hubs])
            end,
            {NReceivedReplies,MaxDistance,Iterations}
    end.

%% deprecated in favor of lazy startup from disk
cachePidAndEnsureExistence(W) ->
    case global:whereis_name(W) of
        P when is_pid(P) ->
            put(W,P),
            W;
        _ ->
            %% kludge or just (good) lazy?  It allows for starting with
            %% a subset of all words (eg those with several connections),
            %% but it's ugly because of the layer-violation
            P = wordnet_to_custom_thing:connectNyms(W,wordnet:lemma(W),true),
            put(W,P),
            W
            %err
    end.

addPlayer(#{occupiers := []} = GS,PlayerId) ->
    {GS#{occupiers => [PlayerId]},[]};
addPlayer(#{occupiers := [PlayerId]} = GS,PlayerId) ->
    {GS,[]}; %% no-op
addPlayer(#{occupiers := Occupiers} = GS,PlayerId) ->
    %% conflict!
    {GS#{occupiers => [PlayerId | Occupiers]},[{conflict,PlayerId,Occupiers}]}.

tick(#{occupiers := []} = GS) -> %,_,_,_,_) ->
    {GS,[]};
tick(#{occupiers := [Owner]} = GS) -> %,A,S,He,Ho) ->
    WordToTry = todo,%util:randFromList(A++S++He++Ho),
    {GS,[{occupy_attempt,Owner,WordToTry}]}.

diseminateSideEffect({conflict,AttackerId,[UndisputedOccupier]}) ->
    lager:warning("conflict! attacker:~p undisputed-occupier:~p",[AttackerId,UndisputedOccupier]);
diseminateSideEffect({conflict,AttackerId,MultipleOccupiers}) ->
    lager:warning("conflict! attacker:~p multiple-occupiers:~p",[AttackerId,MultipleOccupiers]);
diseminateSideEffect({occupy_attempt,AttackerId,WordToTry}) ->
    lager:warning("word:~p (~p) owned by:~p - attempting occupation of word:~p",[get(word),get(lemma),AttackerId,WordToTry]),
    pidOf(WordToTry) ! { occupy, AttackerId };
diseminateSideEffect(Other) ->
    lager:warning("unhandled effect:~p",[Other]).

diseminateSideEffects(Effects) ->
    [ diseminateSideEffect(E) || E <- Effects ].

%% see style.txt - am I weird?
loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState) ->
    receive
        { occupy, PlayerId } ->
            {NewGameState,SideEffects} = addPlayer(GameState,PlayerId),
            diseminateSideEffects(SideEffects),
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,NewGameState);
        { tick, _EpochId } ->
            {NewGameState,SideEffects} = tick(GameState),%,Antonyms,Synonyms,Hypernyms,Hyponyms),
            diseminateSideEffects(SideEffects),
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,NewGameState);
        { shortest_path, TargetWordId, Ref, RequesterPID } ->
            RequesterPID ! {reply, Ref, get({path,TargetWordId})},
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState);
        { distance, TargetWordId, Ref, RequesterPID } ->
            RequesterPID ! {reply, Ref, get({distance,TargetWordId})},
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState);
        { all_connected, _, Ref, RequesterPID } ->
            Island = lists:foldl(fun({{distance,W},_D},Acc) ->
                                         [W|Acc];
                                    (_,Acc) -> Acc
                                 end,[],get()),
            RequesterPID ! { reply, Ref, Island },
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState);
        { all_connected_at_distance, 1, Ref, RequesterPID } ->
            RequesterPID ! { reply, Ref, OtherNyms },
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState);
        { all_connected_at_distance, Distance, Ref, RequesterPID } ->
            DistantWords = lists:foldl(fun({{distance,W},D},Acc) when Distance == D ->
                                               [W|Acc];
                                          (_,Acc) -> Acc
                                       end,[],get()),
            RequesterPID ! { reply, Ref, DistantWords },
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState);
%        { serialize, #Ref<0.0.59.151390>, <0.39.0> }
        { serialize, SRef, SManagerPID } ->
            serializeToDisk(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState),
            SManagerPID ! { serialized, WordId, SRef },
            loop(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState)
        %% { naive_island_explorer, Explored, ToExplore, PIDToReportTo } ->
        %%     OnTheList = Explored ++ ToExplore,
        %%     UnexploredAntonyms = Antonyms -- OnTheList,
        %%     UnexploredSynonyms = Synonyms -- OnTheList,
        %%     UnexploredHypernyms = Hypernyms -- OnTheList,
        %%     UnexploredHyponyms = Hyponyms -- OnTheList,

%        { shortest_path_search,  ->
    after 37000 ->
            erlang:hibernate(?MODULE,loop,[WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState])
    end.

serializeToDisk(WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState) ->
    FN = util:format("data/~p",[WordId]),
    lager:info("serializeToDisk(~p,~p,_OtherNyms,~p,~p,_GS) saving to fn:~p",
               [WordId,Lemma,PartitionSize,MaxHopsToAnywhere,FN]),
    case file:open(FN, [write, raw, binary]) of
        {ok,File} ->
            ProcDict = get(),
            BinState = term_to_binary({WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState,ProcDict}),
            R = case file:write(File,BinState) of
                    {error,WriteErrReason} ->
                        lager:error("serializeToDisk(~p,~p,_OtherNyms,~p,~p,_GameState) failed to write to:~p (~p)!",
                                    [WordId,Lemma,PartitionSize,MaxHopsToAnywhere,FN,WriteErrReason]),
                        err;
                    ok -> ok
                end,
            file:close(File);
        {error,Reason} ->
            lager:error("serializeToDisk(~p,~p,_OtherNyms,~p,~p,_GameState) failed to open:~p (~p)!",
                        [WordId,Lemma,PartitionSize,MaxHopsToAnywhere,FN,Reason]),
            err
    end.

loadFromDisk(WordId) ->
    FN = util:format("data/~p",[WordId]),
    lager:info("loadFromDisk(~p) fn:~p",[WordId,FN]),
    case file:read_file(FN) of
        {ok,BinState} ->
            {WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState,ProcDict}
                = binary_to_term(BinState),
            [ put(K,V) || {K,V} <- ProcDict ],
            erlang:hibernate(?MODULE,loop,[WordId,Lemma,OtherNyms,PartitionSize,MaxHopsToAnywhere,GameState]);
        {error,Reason} ->
            lager:error("loadFromDisk(~p) failed to read_file(~p) reason:~p",
                        [WordId,FN,Reason]),
            err
    end.

rpc(OriginLemma,RequestType,RequestArgs) when is_list(OriginLemma) ->
    W = wordnet:wordId(OriginLemma),
    rpc(W,RequestType,RequestArgs);
rpc(WordId,RequestType,RequestArgs) ->
    case global:whereis_name(WordId) of
        P when is_pid(P) ->
            Ref = make_ref(),
            P ! { RequestType, RequestArgs, Ref, self() },
            receive
                { reply, Ref, Result } ->
                    Result
            after 60000 ->
                    timed_out
            end;
        _ ->
            exit({word_proc_down,WordId})
    end.

shortestPath(Origin,DestinationLemma) when is_list(DestinationLemma) ->
    DW = wordnet:wordId(DestinationLemma),
    shortestPath(Origin,DW);
shortestPath(Origin,DestinationWordId) ->
    rpc(Origin,shortest_path,DestinationWordId).

distance(Origin,DestinationLemma) when is_list(DestinationLemma) ->
    DW = wordnet:wordId(DestinationLemma),
    distance(Origin,DW);
distance(Origin,DestinationWordId) ->
    rpc(Origin,distance,DestinationWordId).

allConnected(Origin) ->
    rpc(Origin,all_connected,na).

connectedAtDistance(Origin,Distance) ->
    rpc(Origin,all_connected_at_distance,Distance).


breadthFirstSearch(Origin,TestFun) ->
    InitialNodesToTest = connectedAtDistance(Origin,1),
    breadthFirstSearchLoop(TestFun,[],[Origin],InitialNodesToTest).

breadthFirstSearchLoop(_TestFun,_TestedNodes,_Path,[]) ->
    not_found;
breadthFirstSearchLoop(TestFun,TestedNodes,Path,NodesToTest) ->
    io:format("breadthFirstSearchLoop(~p,~p,~p,~p)~n",
              [TestFun,TestedNodes,Path,NodesToTest]),
    case testNodes(TestFun,NodesToTest) of
        not_found ->
            breadthFirstSearchSubLoop(TestFun,TestedNodes,Path,NodesToTest);
        {SuccessNode,R} -> {R,Path++[SuccessNode]}
    end.

testNodes(_TestFun,[]) ->
    not_found;
testNodes(TestFun,[N1|NR]) ->
    case TestFun(N1) of
        false ->
            testNodes(TestFun,NR);
        R -> {N1,R}
    end.

breadthFirstSearchSubLoop(_TestFun,_TestedNodes,_Path,[]) ->
    not_found;
breadthFirstSearchSubLoop(TestFun,TestedNodes,Path,[SubNode1|RestOfSubnodes]) ->
    SubSubNodesToTest = [N || N <- connectedAtDistance(SubNode1,1),
                              not lists:member(N,TestedNodes) ],
    case breadthFirstSearchLoop(TestFun,TestedNodes,Path++[SubNode1],SubSubNodesToTest) of
        not_found ->
            breadthFirstSearchSubLoop(TestFun,TestedNodes,Path,RestOfSubnodes);
        R -> R
    end.
