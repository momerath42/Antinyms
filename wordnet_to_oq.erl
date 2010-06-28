-module(wordnet_to_oq).

-compile(export_all).

-define(DB,"antinyms").
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
    case catch oqgraph:create(?DB,Table) of
	{'EXIT',_} ->
	    presumably_exists;
	_ -> ok
    end.

createTables() ->
    createTable("synonyms"),
    createTable("hypernyms"),
    createTable("hyponyms"),
    createTable("antonyms").

init() ->
    createTables(),
    Words = [W || [W] <- wordnet:squery("select wordid from words") ],
    [ connectNyms(Word) || Word <- Words ].

connectNyms(Word) ->
    [ connect("synonyms",Word,W) || W <- wordsLinkedBy(Word,?SYNONYM) ],
    [ connect("hypernyms",Word,W) || W <- wordsLinkedBy(Word,?HYPERNYM) ],
    [ connect("hyponyms",Word,W) || W <- wordsLinkedBy(Word,?HYPONYM) ],
    Antonyms = [ W || [W] <- wordnet:squery("select word2id from lexlinks where word1id = ~p and linkid = ~p",[Word,?ANTONYM]) ],
    [ connect("antonyms",Word,W) || W <- Antonyms ].

