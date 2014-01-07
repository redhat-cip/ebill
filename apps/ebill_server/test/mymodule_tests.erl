%%%'   HEADER
%% @author    Susan Potter <me@susanpotter.net>
%% @copyright 2011 Susan Potter
%% @doc       EUnit test suite module mymodule.
%% @end

-module(mymodule_tests).
-author('Susan Potter <me@susanpotter.net>').

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").

-define(MODNAME, mymodule).
%%%.
%%%' TEST GENERATOR
%% @spec mymodule_test_() -> List
%% where
%%       List = [term()]
mymodule_test_() ->
  %% add your asserts in the returned list, e.g.:
  %% [
  %%   ?assert(?MODNAME:double(2) =:= 4),
  %%   ?assertMatch({ok, Pid}, ?MODNAME:spawn_link()),
  %%   ?assertEqual("ba", ?MODNAME:reverse("ab")),
  %%   ?assertError(badarith, ?MODNAME:divide(X, 0)),
  %%   ?assertExit(normal, ?MODNAME:exit(normal)),
  %%   ?assertThrow({not_found, _}, ?MODNAME:func(unknown_object))
  %% ]
  [].
%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:

