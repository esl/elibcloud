%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Unit tests for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud_test).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").

my_test() ->
  ?assertMatch(ok, ok).

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
