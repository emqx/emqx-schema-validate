-module(emqx_schema_validate).

%% API exports
-export([main/1, do_spellcheck_schema/1]).

-define(DEFAULT_MAX_CONCURRENCY, 10).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    main(Args, ?DEFAULT_MAX_CONCURRENCY).

main(["-j", N0 | Rest], _MaxConcurrency) ->
    N = list_to_integer(N0),
    main(Rest, N);
main(["-"], MaxConcurrency) ->
    Binary = iolist_to_binary(read_stdio()),
    process_data(Binary, MaxConcurrency);
main([JsonFile], MaxConcurrency) ->
    {ok, Binary} = file:read_file(JsonFile),
    process_data(Binary, MaxConcurrency);
main(_, _) ->
    io:format("Usage: emqx_schema_validate <path-to-json-schema-dump>~n", []),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================

process_data(Binary, MaxConcurrency) ->
    {ok, _} = application:ensure_all_started(emqx_schema_validate),
    langtool:start(),
    Data = jsone:decode(Binary, [{object_format, map}, {keys, atom}]),
    spellcheck_schema(Data, MaxConcurrency),
    case is_ok() of
        true -> halt(0);
        false -> halt(1)
    end.

spellcheck_schema(Data, MaxConcurrency) ->
    Chunk = lists:sublist(Data, MaxConcurrency),
    Rest = lists:sublist(Data, MaxConcurrency, length(Data)),
    Refs =
        lists:foldl(
          fun(Node, Acc) ->
             {Pid, Ref} = spawn_monitor(?MODULE, do_spellcheck_schema, [Node]),
             Acc#{{Pid, Ref} => true}
          end,
          #{},
          Chunk),
    spellcheck_schema_loop(Rest, Refs).

spellcheck_schema_loop([] = _Data, Refs) when map_size(Refs) == 0 ->
    ok;
spellcheck_schema_loop([] = Data, Refs0) ->
    receive
        {'DOWN', Ref, process, Pid, _} when is_map_key({Pid, Ref}, Refs0) ->
            Refs = maps:remove({Pid, Ref}, Refs0),
            spellcheck_schema_loop(Data, Refs)
    end;
spellcheck_schema_loop([Node | Rest], Refs0) ->
    receive
        {'DOWN', Ref, process, Pid, _} when is_map_key({Pid, Ref}, Refs0) ->
            Refs1 = maps:remove({Pid, Ref}, Refs0),
            {NPid, NRef} = spawn_monitor(?MODULE, do_spellcheck_schema, [Node]),
            Refs = Refs1#{{NPid, NRef} => true},
            spellcheck_schema_loop(Rest, Refs)
    end.

do_spellcheck_schema(Node = #{full_name := FullName}) ->
    do_spellcheck([FullName], Node),
    Fields = maps:get(fields, Node, []),
    [begin
         FieldName = [FullName, maps:get(name, Field)],
         do_spellcheck(FieldName, Field)
     end || Field <- Fields].

%% Check spelling in any description:
do_spellcheck(FullName, #{desc := Desc}) ->
    Resp = langtool:req(Desc),
    case Resp of
        [] ->
            ok;
        L  ->
            setfail(),
            Header = io_lib:format("!! '~s'~n~n", [format_name(FullName)]),
            Warnings = [io_lib:format("~s", [langtool:format_warning(I)]) || I <- L],
            io:put_chars(user, [Header, Warnings]),
            ok
    end;
%% Ignore references to structs, since the struct itself should have a description
do_spellcheck(_FullName, #{type := #{kind := <<"struct">>}}) ->
    ok;
do_spellcheck(FullName, _) ->
    Record = hd(FullName),
    case binary:match(Record, [<<"Root Config Keys">>]) of
       nomatch ->
          io:format(user, "Error: '~s' doesn't have a description~n", [format_name(FullName)]),
          setfail();
       _ -> ok
    end,
    ok.

read_stdio() ->
    case io:get_chars("", 8096) of
        eof -> [];
        {error, Err} ->
            error({io_error, Err});
        Str ->
            [Str|read_stdio()]
    end.

setfail() ->
    persistent_term:put(?MODULE, true).

is_ok() ->
    persistent_term:get(?MODULE, false) =/= true.

format_name(FullName) ->
    lists:join("::", FullName).
