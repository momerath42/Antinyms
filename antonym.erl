-module(antonym).

-compile(export_all).

-define(UNIT_TIME,30000).

%% TODO: replace the relational territory table with per-territory
%% oqgraphs which are prefilled fully-expanded with 1? weights. that
%% way, I can change occupied territory's weights to 0 and do a
%% weight=1 search to find edge-words (instead of the ridiculously
%% expensive list-shuffling I'm doing here)

start() ->
    wordnet:init(),
    wordnet:fetch("delete from players",[]),
    Listener = startWSListener(),
    io:format("starting listener:~p~n",[Listener]).

startWSListener() ->
    F = fun interact/2,
    spawn(fun() -> websocket_server:start(F,0) end).

interact(Browser, State) ->
    Player = playerIdByBrowser(Browser),
    receive
	{browser, Browser, Str} ->
	    case wordId(Str) of
		not_found ->
		    Browser ! {send, "out ! " ++ "word not found!"};
		Word ->
		    occupy(Player,Word),
		    updatePlayersTerritory(Player,Browser)
	    end,
	    interact(Browser, State)
    after ?UNIT_TIME ->   
	    Browser ! {send, "round ! " ++ integer_to_list(State)},
	    growColonies(Player),
	    updatePlayersTerritory(Player,Browser),
	    ?MODULE:interact(Browser, State+1)
    end.

%% i'm such a sucker for the process dictionary
playerIdByBrowser(PID) ->
    case erlang:get({player,PID}) of
	undefined ->
	    Id = nextPlayerId(),
	    erlang:put({player,PID},Id),
	    erlang:put({browser,Id},PID),
	    Id;
	Id ->
	    Id
    end.

browserByPlayerId(Id) ->
    erlang:get({browser,Id}).

sendToPlayer(Player,Msg) ->    
    Browser = browserByPlayerId(Player),
    Browser ! Msg.
    
updatePlayersTerritory(Player,Browser) ->
    Terr = [ binary_to_list(lemma(W)) || {W,_} <- territory(Player) ],
    Browser ! {send, lists:flatten(io_lib:format("out ! ~p",[Terr]))}.

site_init() ->
    wordnet:fetch("CREATE TABLE territory ("
		  "  id INT UNSIGNED NOT NULL, "
		  "  player INT UNSIGNED NOT NULL, "
		  "  node BIGINT UNSIGNED NOT NULL, "
		  "  PRIMARY KEY (id,player,node) "
		  " ) ENGINE=MYISAM;",[]),
    wordnet:fetch("CREATE TABLE players ("
		  "  player INT UNSIGNED NOT NULL, "
		  "  pid VARCHAR(255), "
		  "  PRIMARY KEY (pid) "
		  " ) ENGINE=MYISAM;",[]).

wordId(WordStr) ->
    case wordnet:squery("select wordid from words where lemma = '~s'",[WordStr]) of
	[[Id]] -> Id;
	_Other ->
	    not_found
    end.

lemma(WordId) ->
    [[R]] = wordnet:squery("select lemma from words where wordid = ~p",[WordId]),
    R.

occupy(Player,Word) ->
    io:format("occupy(~p,~p)~n",[Player,Word]),
    Occupants = occupants(Word),
    case lists:member(Player,Occupants) of
	true ->
	    rejectDuplicate(Player,Word);
	false ->
	    case isNymOfTerritory(Player,Word) of
		{true, Nym, TerritoryId, Relationship} ->
		    case playersConnectedByAntonym(Word) of
			[] ->
			    extendTerritory(Player,Word,TerritoryId,Relationship,Nym,Occupants);
			AntonymConnections ->
			    [ beginWar(P,T,Word,E,ET,Ant) || {P,T} <- Occupants,
							     {E,ET,Ant} <- AntonymConnections ]
		    end;
		false ->
		    case playersConnectedByAntonym(Word) of
			[] ->
			    if Occupants == [] ->
				    beginTerritory(Player,Word);
			       true ->
				    [ rejectColonyStartingPoint(Player,Word,OccupantId) || {OccupantId,_} <- Occupants ]
				    %%OccReward = ?NYM_SPAWN_POINTS / length(Occupants),
				    %%[ rewardForOccupyingSpawnPoint(OccupantId,OccReward) || OccupantId <- Occupants ]
			    end;
			_AntonymConnections ->
			    rejectColonyStartingPoint(Player,Word,1) %% todo: use enemy player(s) to send hint event
			    %%rewardForOccupyingSpawnPoint(EnemyPlayer,?ANTONYM_SPAWN_POINTS)
		    end
	    end
    end.

randFromList(L) ->
    randFromList(L,erlang:length(L)).

randFromList(L,Length) ->
    lists:nth(random:uniform(Length),L).

colonies(Player) ->
    wordnet:squery("select id from territory where player = ~p",[Player]).

growColonies(Player) ->
    io:format("growColonies(~p)~n",[Player]),
    [ growColony(Player,TId) || [TId] <- colonies(Player) ].

growColony(Player,Territory) ->
    case wordnet:squery("select node from territory where player = ~p and id = ~p",[Player,Territory]) of
	[] -> dn;
	Colonies ->
	    Words = [ W || [W] <- Colonies],
	    case lists:usort(lists:flatten([ nyms(Word) || Word <- Words ])) of
		[] -> sendToPlayer(Player,{send,"debug ! you've maxed out this potential territory!"});
		Nyms ->
		    NewWord = randFromList(Nyms),
		    occupy(Player,NewWord)
	    end
    end.

nyms(Word) ->
    synonyms(Word)++hypernyms(Word)++hyponyms(Word).

synonyms(Word) ->
    oqgraph:findDirectlyConnected("synonyms",Word).

hypernyms(Word) ->
    oqgraph:findDirectlyConnected("hypernyms",Word).

hyponyms(Word) ->
    oqgraph:findDirectlyConnected("hyponyms",Word).

antonyms(Word) ->
    oqgraph:findDirectlyConnected("antonyms",Word).

	    
occupants(Word) ->
    [ {P,T} || [P,T] <- wordnet:squery("select player,id from territory where node = ~p",[Word]) ].

rejectDuplicate(Player,Word) ->				
    todo,
    io:format("rejecting ~p from ~p~n",[Word,Player]).

areDirectlyConnected(W1,W2) ->
    io:format("areDirectlyConnected(~p,~p)~n",[W1,W2]),
    case oqgraph:areDirectlyConnected("synonyms",W1,W2) of
	true -> {true,synonym};
	false ->	
	    case oqgraph:areDirectlyConnected("hypernyms",W1,W2) of
		true -> {true,hyperonym};
		false ->
		    case oqgraph:areDirectlyConnected("hyponyms",W1,W2) of
			true -> {true,hypoonym};
			false -> false
		    end
	    end
    end.

territory(Player) ->
    [ {W,T} || [W,T] <- wordnet:squery("select node,id from territory where player = ~p",[Player]) ].

%isNymOfTerritory(Player,Word) ->

    
isNymOfTerritory(Player,Word) ->
    io:format("isNymOfTerritory(~p,~p)~n",[Player,Word]),
    TerritoryTestNodes = territory(Player),
    lists:foldl(fun({W,T},false) ->
			case areDirectlyConnected(W,Word) of
			    {true,FirstNymType} -> {true,W,T,FirstNymType};
			    false -> false
			end;
		   (_,Acc) ->
			Acc
		end,false,TerritoryTestNodes).
			      
playersConnectedByAntonym(Word) ->
    lists:foldl(fun(Ant,Acc) ->
			case occupants(Ant) of
			    [] -> Acc;
			    [{Enemy,Territory}] -> [{Enemy,Territory,Ant} | Acc]
			end
		end,[],antonyms(Word)).
	
extendTerritory(Player,Word,TerritoryId,_Relationship,Nym,Occupants) ->
    wordnet:fetch("insert into territory (player,id,node) values (~p,~p,~p)",[Player,TerritoryId,Word]),
    wordnet:fetch("insert into territory~p (origid,destid) values (~p,~p),(~p,~p)",[Player,Word,Nym,Nym,Word]),
    %% sendGraphUpdate(Player,Word,Nym),
    [ informOfNewOccupant(P,Player,Word) || {P,_} <- Occupants ].

beginWar(P,T,Word,E,ET,Ant) ->
    PTSize = wordnet:squery("select count(*) from territory where player = ~p and id = ~p",[P,T]),
    ETSize = wordnet:squery("select count(*) from territory where player = ~p and id = ~p",[E,ET]),
    {Winner,_WT,Loser,LT} = if PTSize > ETSize -> {P,T,E,ET};
			      true -> {E,ET,P,T}
			   end,
    wordnet:fetch("update territory set player = ~p where id = ~p",[Winner,LT]),
    WordsChangingHands = wordnet:squery("select node from territory where id = ~p",[LT]),
    if P == Winner ->
	    ToInstigator = io_lib:format("You played:~p against:~p and won:~p from:~p~n",[Word,Ant,WordsChangingHands,Loser]),
	    sendToPlayer(Winner,{send,"debug ! " ++ ToInstigator}),
	    ToVictim = io_lib:format("~p played:~p against:~p and won:~p from you.~n",[Winner,Word,Ant,WordsChangingHands]),
	    sendToPlayer(Loser,{send,"debug ! " ++ ToVictim}),
	    io:format("~p was attacked using:~p against:~p and lost:~p to:~p~n",
		      [Loser,Word,Ant,WordsChangingHands,Winner]);
       true ->
	    ToInstigator = io_lib:format("You played:~p against:~p and lost:~p to:~p~n",[Word,Ant,WordsChangingHands,Loser]),
	    sendToPlayer(Loser,{send,"debug ! " ++ ToInstigator}),
	    ToVictoriousVictim = io_lib:format("~p played:~p against:~p and won:~p from you.~n",[Winner,Word,Ant,WordsChangingHands]),
	    sendToPlayer(Winner,{send,"debug ! " ++ ToVictoriousVictim}),
	    io:format("~p was attacked using:~p against:~p but came out victorious with:~p from:~p~n",
		      [Winner,Word,Ant,WordsChangingHands,Loser])
    end.
%    wordnet:squery("select node from territory where player = ~p and id = ~p",[Loser,LT]),
%    [ extendTerritory(Winner,Word,WT,fuck,Ant,

nextTerritoryId() ->
    nextId(territory,fun() -> wordnet:squery("select id from territory order by id desc limit 1") end).

nextPlayerId() ->
    io:format("nextPlayerId()~n"),
    nextId(player,fun() -> 1 end).

nextId(IdName,InitFun) ->
    Id = case erlang:get(IdName) of
	     undefined ->
		 case InitFun() of
		     N when is_integer(N) ->
			 N;
		     [] ->
			 1;
		     [[N]] ->
			 N+1
		 end;
	     N ->
		 N+1
	 end,
    erlang:put(IdName,Id),
    Id.
			     
beginTerritory(PlayerId,Word) ->
    NewTerritoryId = nextTerritoryId(),
    wordnet:fetch("insert into territory (player,id,node) values (~p,~p,~p)",[PlayerId,NewTerritoryId,Word]),
    NewTerritoryId.

rejectColonyStartingPoint(_,_,_) ->
    todo.

informOfNewOccupant(_,_,_) ->
    todo.

    
%% between commits attic


persistent_playerIdByBrowser(PID) ->
    case wordnet:squery("select player from players where pid = '~s'",[pid_to_list(PID)]) of
	[] ->
	    Id = nextPlayerId(),
	    wordnet:fetch("insert into players (player,pid) values (~p,'~s')",[Id,pid_to_list(PID)]),
	    Id;
	[[Id]] ->
	    Id
    end.

%% unecessary but convenient for trying new things without commiting to dragging the variable down from interact
persistent_browserByPlayerId(Player) ->
    case wordnet:squery("select pid from players where player = ~p~n",[Player]) of
	[] ->
	    io:format("can't find browser for player:~p!!~n",[Player]);
	[[PIDStr]] ->
	    list_to_pid(binary_to_list(PIDStr))
    end.

