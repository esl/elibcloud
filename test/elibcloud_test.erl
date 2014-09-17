%%%=============================================================================
%%% Copyright (C) 2014, Erlang Solution Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc Unit tests for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud_test).

-include_lib("eunit/include/eunit.hrl").

create_node_test() ->
    Res = elibcloud:create_node(
            _Credentials = credentials(),
            _NodeName = "my_nodename",
            _SizeId = "1",
            _ImageId = "1",
            _KeyName = "my_key_name",
            _SecurityGroupNames = []),

    ?assertMatch({ok, _Json}, Res),
    {ok, Json} = Res,

    ?assertMatch([{<<"id">>, _NodeId},
                  {<<"publicIps">>, [<<"127.0.0.3">>]}], lists:sort(Json)),
    [{<<"id">>, NodeId}, _PublicIPs] = lists:sort(Json),

    ?assert(is_binary(NodeId)).

create_node_no_such_size_test() ->
    Res = elibcloud:create_node(
            _Credentials = credentials(),
            _NodeName = "my_nodename",
            _SizeId = "my_sizeid",
            _ImageId = "my_imageid",
            _KeyName = "my_key_name",
            _SecurityGroupNames = []),

    ?assertMatch({error, {no_such_size, _Details}}, Res),
    {error, {no_such_size, Details}} = Res,

    ?assertMatch([{<<"error">>, <<"no_such_size">>},
                  {<<"size_id">>, <<"my_sizeid">>}], lists:sort(Details)).

create_node_no_such_image_test() ->
    Res = elibcloud:create_node(
            _Credentials = credentials(),
            _NodeName = "my_nodename",
            _SizeId = "1",
            _ImageId = "my_imageid",
            _KeyName = "my_key_name",
            _SecurityGroupNames = []),

    ?assertMatch({error, {no_such_image, _Details}}, Res),
    {error, {no_such_image, Details}} = Res,

    ?assertMatch([{<<"error">>, <<"no_such_image">>},
                  {<<"image_id">>, <<"my_imageid">>}], lists:sort(Details)).

destroy_node_test() ->

    Res = elibcloud:destroy_node(
            _Credentials = credentials(),
            {id, _NodeId = "1"}),

    ?assertMatch({ok, [{}]}, Res).

destroy_non_existent_node_test() ->
    Res = elibcloud:destroy_node(
            _Credentials = credentials(),
            {id, _NodeId = "my_nodeid"}),

    ?assertMatch({error, {no_such_node, _Details}}, Res),
    {error, {no_such_node, Details}} = Res,

    ?assertMatch([{<<"error">>, <<"no_such_node">>},
                  {<<"node_id">>, <<"my_nodeid">>}], lists:sort(Details)).

my_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"empty", fun empty/0}
     ]}.

setup() ->
    ok.

teardown(_) ->
    ok.

empty() ->
    ?_assertMatch(ok, ok).

credentials() ->
    {ok, Cred} = elibcloud:create_credentials(_Provider = "DUMMY",
                                              _UserName = "my_username",
                                              _Password = "my_password"),
    Cred.
