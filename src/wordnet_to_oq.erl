-module(wordnet_to_oq).

-compile(export_all).

-define(SYNONYM,40).
-define(HYPERNYM,1).
-define(HYPONYM,2).
-define(ANTONYM,30).

-import(oqgraph, [connect/3]).

wordIdOfSynSet(SynId) ->
    %% todo: make sure taking the first one doesnt fuck shit up
    [[WordId]|_] = wordnet:squery("select wordid from wordsXsensesXsynsets where synsetid = ~p",[SynId]),
    WordId.

wordsLinkedBy(Word,LinkId) ->
    [ wordIdOfSynSet(SynId) || [SynId] <- wordnet:squery("select synset2id from semlinks, wordsXsensesXsynsets where linkid = ~p and synsetid = synset1id and wordid = ~p",[LinkId, Word]) ].

createTable(Table) ->
    case catch oqgraph:create(Table) of
	{'EXIT',_} -> %% bad style
	    presumably_exists;
	_ -> ok
    end.

createTables() ->
    createTable("synonyms"),
    createTable("hypernyms"),
    createTable("hyponyms"),
    createTable("nyms"), %% syn+hyper+hypo
    createTable("antonyms").

init() ->
    createTables(),
    Words = [W || [W] <- wordnet:squery("select wordid from words") ],
    [ connectNyms(Word) || Word <- Words ].

connectNyms(Word) ->
    Syn = [ connect("synonyms",Word,W) || W <- wordsLinkedBy(Word,?SYNONYM) ],
    Hyper = [ connect("hypernyms",Word,W) || W <- wordsLinkedBy(Word,?HYPERNYM) ],
    Hypo = [ connect("hyponyms",Word,W) || W <- wordsLinkedBy(Word,?HYPONYM) ],
    [ connect("nyms",W1,W2) || {W1,W2} <- Syn++Hyper++Hypo ],
    %% for my island finding, I need anything that can be reached in one direction to be reachable in the other (no knots)
    [ connect("nyms",W2,W1) || {W1,W2} <- Syn++Hyper++Hypo ],
    Antonyms = [ W || [W] <- wordnet:squery("select word2id from lexlinks where word1id = ~p and linkid = ~p",[Word,?ANTONYM]) ],
    [ connect("antonyms",Word,W) || W <- Antonyms ].

