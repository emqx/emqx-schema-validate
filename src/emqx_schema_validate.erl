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
    Data = jsone:decode(Binary, [{object_format, map}, {keys, atom}]),
    spellcheck_schema(Data),
    case is_ok() of
        true -> halt(0);
        false -> halt(1)
    end.

spellcheck_schema(Data) ->
    [begin
         do_spellcheck([FullName], Node),
         Fields = maps:get(fields, Node, []),
         [begin
              FieldName = [FullName, maps:get(name, Field)],
              do_spellcheck(FieldName, Field)
          end || Field <- Fields]
     end || Node = #{full_name := FullName} <- Data],
    ok.

%% Check spelling in any description:
do_spellcheck(FullName, #{desc := Desc}) ->
    Resp = langtool:req(Desc),
    case Resp of
        [] ->
            ok;
        L  ->
            setfail(),
            io:format(user, "!! '~s'~n~n", [format_name(FullName)]),
            [io:format(user, "~s", [langtool:format_warning(I)]) || I <- L],
            ok
    end;
%% Ignore references to structs, since the struct itself should have a description
do_spellcheck(FullName, #{type := #{kind := <<"struct">>}}) ->
    ok;
do_spellcheck([<<"Root Config Keys">>], _) ->
    ok;
do_spellcheck(FullName, _) ->
    Record = hd(FullName),
    io:format(user, "Error: '~s' doesn't have a description~n", [format_name(FullName)]),
    setfail(),
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
    put(?MODULE, true).

is_ok() ->
    get(?MODULE) =/= true.

format_name(FullName) ->
    lists:join("::", FullName).
