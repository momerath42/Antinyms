-module(oqgraph).

-compile(export_all).


init() ->
    mysql:start_link(mconn, "localhost", "antinyms", "s00p3rs3cr3t", "antinyms").

findDirectlyConnected(Table,Origin) ->
    [ W || [W] <- squery("select linkid from ~s where latch = 1 and origid = ~p and weight = 1",[Table,Origin]) ].

findReachable(Table,Origin) ->
    squery("select linkid from ~s where latch = 1 and origid = ~p",[Table,Origin]).

shortestPath(Table,OrigId,DestId) ->
    squery("select linkid from ~s where latch = 2 and origid = ~p and destid = ~p",[Table,OrigId,DestId]).

areConnected(Table,OrigId,DestId) ->
    shortestPath(Table,OrigId,DestId) =/= [].

areDirectlyConnected(Table,OrigId,DestId) ->
    squery("select linkid from ~s where latch = 2 and origid = ~p and destid = ~p and weight = 1",[Table,OrigId,DestId]) =/= [].

squery(SQLQuery) ->
    {data,R} = mysql:fetch(mconn,SQLQuery),
    mysql:get_result_rows(R).

squery(SQLFormatStr,Args) ->
    squery(lists:flatten(io_lib:format(SQLFormatStr,Args))).

connect(Table,Id1,Id2) ->
    case mysql:fetch(mconn,lists:flatten(io_lib:format("insert into ~s (origid,destid) values (~p,~p)",[Table,Id1,Id2]))) of
	{error,{mysql_result,[],[],0,0,Err}} ->
	    io:format("dupe!?: ~p ~p ~p (~p)~n",[Table,Id1,Id2,Err]),
	    dupe;
	{error,{mysql_result,[],[],0,Err}} ->
	    io:format("dupe!?: ~p ~p ~p (~p)~n",[Table,Id1,Id2,Err]),
	    dupe;
	{error,Err} ->
	    exit({connect,Err});
	_ -> {Id1,Id2}
    end.

create(TableName) ->
    mysql:fetch(mconn,lists:flatten(io_lib:format("CREATE TABLE ~s ("
						  "    latch   SMALLINT  UNSIGNED NULL,"
						  "    origid  BIGINT    UNSIGNED NULL,"
						  "    destid  BIGINT    UNSIGNED NULL,"
						  "    weight  DOUBLE    NULL,"
						  "    seq     BIGINT    UNSIGNED NULL,"
						  "    linkid  BIGINT    UNSIGNED NULL,"
						  "    KEY (latch, origid, destid) USING HASH,"
						  "    KEY (latch, destid, origid) USING HASH"
						  "  ) ENGINE=OQGRAPH;",[TableName]))).

