-module(emqx_schema_validate).

%% API exports
-export([main/1]).

-define(TAB, stats_tab).

%%====================================================================
%% API functions
%%====================================================================

main(["-"]) ->
    Binary = iolist_to_binary(read_stdio()),
    process_data(Binary);
main([JsonFile]) ->
    {ok, Binary} = file:read_file(JsonFile),
    process_data(Binary);
main(_) ->
    io:format("Usage: emqx_schema_validate <path-to-json-schema-dump>~n", []),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================

process_data(Binary) ->
    {ok, _} = application:ensure_all_started(emqx_schema_validate),
    langtool:start(),
    ets:new(?TAB, [named_table, {keypos,1}]),
    Data = jsone:decode(Binary, [{object_format, map}, {keys, atom}]),
    spellcheck_schema(Data),
    format_undocumented_stats(),
    case is_ok() of
        true -> halt(0);
        false -> halt(1)
    end.

format_undocumented_stats() ->
    Stats = lists:keysort(2, ets:tab2list(?TAB)),
    {_, Vals} = lists:unzip(Stats),
    Total = lists:sum(Vals),
    io:format(user, "~nUndocumented values ~p:~n", [Total]),
    [io:format(user, "~-40.. s ~p~n", [Root, Val]) || {Root, Val} <- lists:reverse(Stats)].

spellcheck_schema(Data) ->
    [do_spellcheck(FullName, I)
     || #{full_name := FullName, fields := Fields} <- Data, I <- Fields],
    ok.

do_spellcheck(FullName, #{name := Name, desc := Desc}) ->
    Resp = langtool:req(Desc),
    case Resp of
        [] ->
            [];
        L  ->
            setfail(),
            io:format(user, "!! '~s'::~s~n~n", [FullName, Name]),
            [io:format(user, "~s", [langtool:format_warning(I)]) || I <- L],
            []
    end;
do_spellcheck(FullName, #{name := Name}) ->
    ets:update_counter(?TAB, FullName, {2, 1}, {FullName, 0}),
    io:format(user, "Warning: ~s.~s field doesn't have a description~n", [FullName, Name]),
    [{FullName, Name}].

read_stdio() ->
    case io:get_chars("", 8096) of
        eof -> [];
        {error, Err} ->
            error({io_error, Err});
        Str ->
            [Str|read_stdio()]
    end.

setfail() ->
    put(?MODULE, true).

is_ok() ->
    get(?MODULE) =/= true.
