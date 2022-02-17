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

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

start() ->
    exec("docker create --name langtool -p 17910:8010 mylangtool"),
    0 = exec("docker start langtool"),
    wait_langtool(20).

stop() ->
    %%exec("docker stop langtool"),
    ok.

-spec req(string()) -> [map()].
req(Text) ->
    Annotation = text_to_data(Text),
    Payload = {form, [{language, <<"en-US">>},
                      {data, Annotation}]},
    Headers = [],
    URL = <<"http://localhost:17910/v2/check">>,
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
      rule := #{description := Description},
      replacements := Repl0
     } = Warn,
    Repl1 = lists:sublist([I || #{value := I} <- Repl0], 5),
    Underscore = [[$  || _ <- lists:seq(1, Offset)], [$~ || _ <- lists:seq(1, Length)]],
    Replacements = lists:join(", ", Repl1),
    io_lib:format("~s~n~s~nDescription: ~s~nReplacements: ~s~n",
                  [Context, Underscore, Description, Replacements]).

%%================================================================================
%% Internal functions
%%================================================================================

text_to_data(Text) ->
    {_, Payload} = trane:sax(<<"<p>", Text/binary, "</p>">>, fun scan_html/2, {false, []}),
    jsone:encode(#{annotation => lists:reverse(Payload)}).

-record(s,
        { skip :: boolean()
        , acc  :: list()
        }).

scan_html({text, Txt}, {false, Acc}) ->
    {false, [#{text => Txt}|Acc]};
scan_html({tag, <<"br">>, _}, {false, Acc}) ->
    {false, [#{text => <<"\n">>}|Acc]};
scan_html({tag, <<"code">>, _}, {false, Acc}) ->
    {true, Acc};
scan_html({end_tag, <<"code">>}, {true, Acc}) ->
    {false, Acc};
scan_html(_Skip, Acc) ->
    Acc.

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
