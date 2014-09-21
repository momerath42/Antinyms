-module(performance).

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

-define(SIGNIFICANT_STEP,10000000).% ~10M
-define(MILLISECONDS_BETWEEN_CHECKS,10000).

start() ->
    register(?MODULE,spawn(?MODULE,loop,[0])).

loop(HighMem) ->
%    timer:sleep(1000),
    receive
        { report, RequesterPid } ->
            RequesterPid ! HighMem
    after ?MILLISECONDS_BETWEEN_CHECKS ->
            NStats = erlang:memory(),
            Total = proplists:get_value(total,NStats),
            if Total > (HighMem + ?SIGNIFICANT_STEP) ->
                    lager:warning("new high watermark in memory:~p",[Total]),
                    loop(Total);
               true -> loop(HighMem)
            end
    end.

report() ->
    ?MODULE ! { report, self() },
    receive
        N ->
            N
    after 60000 ->
            timed_out
    end.

blueDictSize() ->
    BlueId = wordnet:wordId("blue"),
    P = global:whereis_name(BlueId),
    {dictionary,D} = process_info(P,dictionary),
    byte_size(term_to_binary(D)).
