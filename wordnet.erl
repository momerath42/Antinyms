-module(wordnet).

-compile(export_all).

init() ->
    mysql:start_link(mconn, "localhost", "antonym", "s00p3rs3cr3t", "antonym").

squery(SQLQuery) ->
    {data,R} = mysql:fetch(mconn,SQLQuery),
    mysql:get_result_rows(R).

squery(SQLFormatStr,Args) ->
    squery(lists:flatten(io_lib:format(SQLFormatStr,Args))).

fetch(SQLFormatStr,Args) ->
    mysql:fetch(mconn,lists:flatten(io_lib:format(SQLFormatStr,Args))).
    
