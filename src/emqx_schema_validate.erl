-module(emqx_schema_validate).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main([JsonFile]) ->
    {ok, _} = application:ensure_all_started(emqx_schema_validate),
    {ok, Binary} = file:read_file(JsonFile),
    Data = jsone:decode(Binary, [{object_format, map}, {keys, atom}]),
    spellcheck_schema(Data),
    erlang:halt(0);
main(_) ->
    io:format("Usage: emqx_schema_validate <path-to-json-dump>~n", []),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================

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
    io:format(user, "~s::~s~n~p~n", [FullName, Name, Resp]),
    ok;
do_spellcheck(FullName, #{name := Name}) ->
    io:format(user, "Warning: ~s.~s field doesn't have a description~n", [FullName, Name]).
