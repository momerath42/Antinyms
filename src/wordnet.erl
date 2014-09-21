-module(wordnet).

-compile(export_all).

init() ->
    mysql:start_link(mconn, "localhost", undefined, "antinyms", "s00p3rs3cr3t", "antinyms", fun(_Mod,_Line,_Level,_FmtFun) -> dn end).

squery(SQLQuery) ->
    {data,R} = mysql:fetch(mconn,SQLQuery),
    mysql:get_result_rows(R).

squery(SQLFormatStr,Args) ->
    squery(lists:flatten(io_lib:format(SQLFormatStr,Args))).

fetch(SQLFormatStr) ->
    fetch(SQLFormatStr,[]).

fetch(SQLFormatStr,Args) ->
    mysql:fetch(mconn,lists:flatten(io_lib:format(SQLFormatStr,Args))).

wordId(WordStr) ->
    case wordnet:squery("select wordid from words where lemma = '~s'",[WordStr]) of
	[[Id]] -> Id;
	_Other ->
	    not_found
    end.

lemma(WordId) ->
    [[R]] = wordnet:squery("select lemma from words where wordid = ~p",[WordId]),
    R.

lemmas(WordIdList) ->
    [ lemma(W) || W <- WordIdList ].
