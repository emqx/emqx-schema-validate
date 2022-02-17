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
-export([start/0, stop/0, req/1]).

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

-spec req(string()) -> _.
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

%%================================================================================
%% Internal functions
%%================================================================================

text_to_data(Text) ->
    Fun = fun({text, Txt}, Acc) ->
                  [#{text => Txt}|Acc];
             ({tag, Tag, _}, Acc) ->
                  [#{markup => <<"">>}|Acc];
             ({end_tag, Tag}, Acc) ->
                  [#{markup => <<"">>}|Acc]
          end,
    Payload = trane:sax(<<"<p>", Text/binary, "</p>">>, Fun, []),
    jsone:encode(#{annotation => lists:reverse(Payload)}).

wait_langtool(0) ->
    req(<<"Testing...">>);
wait_langtool(N) ->
    try req(<<"Testing...">>) of
        _ -> ok
    catch
        _:Err ->
            ?LOG_DEBUG("Langtool request issue: ~p", [Err]),
            timer:sleep(10),
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
