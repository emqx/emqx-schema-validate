-module(emqx_schema_validate).

%% API exports
-export([main/1]).

-define(TAB, stats_tab).

%%====================================================================
%% API functions
%%====================================================================

main([JsonFile]) ->
    {ok, _} = application:ensure_all_started(emqx_schema_validate),
    ets:new(?TAB, [named_table, {keypos,1}]),
    {ok, Binary} = file:read_file(JsonFile),
    Data = jsone:decode(Binary, [{object_format, map}, {keys, atom}]),
    spellcheck_schema(Data),
    format_undocumented_stats();
main(_) ->
    io:format("Usage: emqx_schema_validate <path-to-json-dump>~n", []),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================

format_undocumented_stats() ->
    Stats = lists:keysort(2, ets:tab2list(?TAB)),
    {_, Vals} = lists:unzip(Stats),
    Total = lists:sum(Vals),
    io:format(user, "~nUndocumented values ~p:~n", [Total]),
    [io:format(user, "~-40.. s ~p~n", [Root, Val]) || {Root, Val} <- lists:reverse(Stats)],
    if Total > 0 ->
            halt(1);
       true ->
            halt(0)
    end.

spellcheck_schema(Data) ->
    try
        langtool:start(),
        [do_spellcheck(FullName, I)
         || #{full_name := FullName, fields := Fields} <- Data
          , I <- Fields],
        ok
    after
        langtool:stop(),
        ok
    end.

do_spellcheck(FullName, #{name := Name, desc := Desc}) ->
    Resp = langtool:req(Desc),
    case Resp of
        [] ->
            [];
        L  ->
            io:format(user, "!! '~s'::~s~n~n", [FullName, Name]),
            [io:format(user, "~s", [langtool:format_warning(I)]) || I <- L],
            []
    end;
do_spellcheck(FullName, #{name := Name}) ->
    ets:update_counter(?TAB, FullName, {2, 1}, {FullName, 0}),
    io:format(user, "Warning: ~s.~s field doesn't have a description~n", [FullName, Name]),
    [{FullName, Name}].
