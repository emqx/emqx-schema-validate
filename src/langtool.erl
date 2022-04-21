%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(langtool).

%% API:
-export([start/0, stop/0, req/1, format_warning/1]).

%% debug:
-export([text_to_data/1]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

start() ->
    wait_langtool(300).

stop() ->
    ok.

-spec req(string()) -> [map()].
req(Text) ->
    Annotation = text_to_data(Text),
    Req = jsone:encode(#{annotation => Annotation}),
    Payload = {form, [{language, <<"en-US">>},
                      {data, Req},
                      {disabledCategories, <<"TYPOGRAPHY">>}]},
    Headers = [],
    URL = application:get_env(emqx_schema_validate, langtool_url,
                              <<"http://localhost:8010/v2/check">>),
    Options = [],
    {ok, Code, _RespHeaders, ClientRef} = hackney:post(URL,
                                                       Headers, Payload,
                                                       Options),
    {ok, Body} = hackney:body(ClientRef),
    {200, _, _} = {Code, Annotation, Body}, %% Assert
    #{matches := Matches} = jsone:decode(Body, [{object_format, map}, {keys, atom}]),
    Matches.

-spec format_warning(map()) -> iolist().
format_warning(Warn) ->
    #{context :=  #{text := Context, length := Length, offset := Offset},
      rule := #{description := Description, category := #{id := Cat}},
      replacements := Repl0
     } = Warn,
    Repl1 = lists:sublist([I || #{value := I} <- Repl0], 5),
    Underscore = [[$  || _ <- lists:seq(1, Offset)], [$~ || _ <- lists:seq(1, Length)]],
    Replacements = lists:join(", ", Repl1),
    io_lib:format("~s~n~s~nError: (~s) ~s~nReplacements: ~s~n",
                  [Context, Underscore, Cat, Description, Replacements]).

%%================================================================================
%% Internal functions
%%================================================================================

-record(s,
        { n   = 0  :: non_neg_integer() % Number of nested code blocks
        , m   = 0  :: 0..1 % Number of backtics
        , acc = [] :: [map()]
        }).

text_to_data(Text) ->
    #s{acc = Payload} = trane:sax(<<"<p>", Text/binary, "</p>">>, fun scan/2, #s{}),
    lists:reverse(Payload).

scan({text, Txt}, S) ->
    Chunks = string:split(Txt, "`", all),
    scan_text(Chunks, S, false);
scan({tag, "code", _}, S) ->
    start_code_block("<code>", S);
scan({end_tag, "code"}, S) ->
    end_code_block(<<"</code>">>, S);
scan({tag, "br", _}, S = #s{n = 0, acc = Acc}) ->
    S#s{acc = [#{markup => <<"<br>">>, interpretAs => <<"\n">>}|Acc]};
scan(OtherTag, S = #s{n = N, acc = Acc}) when N > 0 ->
    S#s{acc = [fmt_tag(OtherTag)|Acc]};
scan(_, S = #s{n = 0, acc = Acc}) ->
    S#s{acc = Acc}.

scan_text([], S, _) ->
    S;
scan_text([Chunk|Rest], S0 = #s{m = M}, Increase) ->
    S1 = if Increase andalso M =:= 0 ->
                 start_code_block(<<"`">>, S0#s{m = 1});
            Increase andalso M =:= 1 ->
                 end_code_block(<<"`">>, S0#s{m = 0});
            true ->
                 S0
         end,
    S = push_chunk(Chunk, S1),
    scan_text(Rest, S, true).

push_chunk(<<>>, S) ->
    S;
push_chunk(Chunk, S = #s{n = 0, acc = Acc}) ->
    S#s{acc = [#{text => Chunk}|Acc]};
push_chunk(Chunk, S = #s{acc = Acc}) ->
    S#s{acc = [#{markup => Chunk}|Acc]}.

start_code_block(Markup, S = #s{n = 0, acc = Acc}) ->
    It = #{markup => Markup, interpretAs => <<" Code ">>},
    S#s{acc = [It|Acc], n = 1};
start_code_block(Markup, S = #s{n = N, acc = Acc}) ->
    It = #{markup => Markup},
    S#s{acc = [It|Acc], n = N + 1}.

end_code_block(Markup, S = #s{n = N, acc = Acc}) ->
    It = #{markup => Markup},
    S#s{acc = [It|Acc], n = max(N - 1, 0)}.

fmt_tag({tag, Tag, _Attrs}) ->
    #{markup => iolist_to_binary(["<", Tag, ">"])};
fmt_tag({end_tag, Tag}) ->
    #{markup => iolist_to_binary(["</", Tag, ">"])}.

wait_langtool(0) ->
    req(<<"Testing...">>);
wait_langtool(N) ->
    try req(<<"Testing...">>) of
        _ -> ok
    catch
        _:Err ->
            ?LOG_DEBUG("Langtool request issue: ~p", [Err]),
            timer:sleep(100),
            wait_langtool(N - 1)
    end.

-spec exec(string()) -> integer().
exec(CMD) ->
  Port = open_port({spawn, CMD},
                   [exit_status,
                    binary,
                    stderr_to_stdout,
                    {line, 300}
                   ]),
  collect_port_output(Port, "docker").

-spec collect_port_output(port(), string()) -> integer().
collect_port_output(Port, CMD) ->
  receive
    {Port, {data, {_, Data}}} ->
      io:format("~s: ~s~n", [CMD, Data]),
      collect_port_output(Port, CMD);
    {Port, {exit_status, ExitStatus}} ->
      ExitStatus
  end.
