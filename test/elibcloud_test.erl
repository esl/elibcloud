%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Unit tests for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud_test).
-copyright("2014, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").

create_instance_test() ->
    {ok, Res} = elibcloud:create_instance(
                  _Provider = "DUMMY",
                  _UserName = "my_username",
                  _Password = "my_password",
                  _NodeName = "my_nodename",
                  _SizeId = "1",
                  _ImageId = "1",
                  _PubKeyName = "my_pubkey_name",
                  _PubKeyData = "my_pubkey_data",
                  _Firewalls = []),
    ?assertMatch([{<<"debug">>, _},
                  {<<"id">>, _NodeId},
                  {<<"publicIps">>, [<<"127.0.0.3">>]}], lists:sort(Res)),
    [_Debug,
     {<<"id">>, NodeId},
     _PublicIPs] = lists:sort(Res),
    ?assert(is_binary(NodeId)).

create_instance_no_such_size_test() ->
    {error, Output} = elibcloud:create_instance(
                        _Provider = "DUMMY",
                        _UserName = "my_username",
                        _Password = "my_password",
                        _NodeName = "my_nodename",
                        _SizeId = "my_sizeid",
                        _ImageId = "my_imageid",
                        _PubKeyName = "my_pubkey_name",
                        _PubKeyData = "my_pubkey_data",
                        _Firewalls = []),
    ?assertEqual(
       "No such size: my_sizeid\n",
       lists:flatten(Output)).

create_instance_no_such_image_test() ->
    {error, Output} = elibcloud:create_instance(
                        _Provider = "DUMMY",
                        _UserName = "my_username",
                        _Password = "my_password",
                        _NodeName = "my_nodename",
                        _SizeId = "1",
                        _ImageId = "my_imageid",
                        _PubKeyName = "my_pubkey_name",
                        _PubKeyData = "my_pubkey_data",
                        _Firewalls = []),
    ?assertEqual(
       "No such image: my_imageid\n",
       lists:flatten(Output)).

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
