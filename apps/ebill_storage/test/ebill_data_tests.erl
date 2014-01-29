-module(ebill_data_tests).

-include_lib("eunit/include/eunit.hrl").

tmdb_test_() ->
  {setup,
    fun setup/0, fun teardown/1,
    [
      ?_test(add()),
      ?_test(search())
    ]}.

% Tests

add() ->
  {Res1, Doc1} = ebill_data:add(<<"test_id">>, <<"test_resource">>, 'cpu.usage', 100),
  ?assertEqual(ok, Res1),
  {Res2, _} = ebill_data:del(Doc1),
  ?assertEqual(ok, Res2).

search() ->
  ?assertEqual(ok, ok).

% Helpers

setup() ->
  ebill_storage:start().

teardown(_) ->
  ok.
