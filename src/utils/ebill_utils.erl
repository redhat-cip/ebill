-module(ebill_utils).

-export([
    expand_path/1,
    user_home/0,
    normalize_path/1
  ]).

expand_path(Path) ->
  normalize_path(filename:absname(expand_home(Path))).

normalize_path(Path) ->
  normalize_path(filename:split(Path), []).
normalize_path([".."|T], []) ->
  normalize_path(T, []);
normalize_path([".."|T], [_|Acc]) ->
  normalize_path(T, Acc);
normalize_path(["."|T], Acc) ->
  normalize_path(T, Acc);
normalize_path([H|T], Acc) ->
  normalize_path(T, [H|Acc]);
normalize_path([], Acc) ->
  filename:join(lists:reverse(Acc)).

expand_home([$~|Rest]) ->
  user_home() ++ Rest;
expand_home(Path) -> Path.

user_home() ->
  case os:type() of
    {win32, _} -> get_windows_home();
    _ -> get_unix_home()
  end.

get_unix_home() ->
  os:getenv("HOME").

get_windows_home() ->
  filename:absname(
    case os:getenv("USERPROFILE") of
      false -> 
        get_windows_home(os:getenv("HOMEDRIVE"));
      Path -> Path
    end
  ).
get_windows_home(false) -> false;
get_windows_home(HomeDrive) -> get_windows_home(HomeDrive, os:getenv("HOMEPATH")).
get_windows_home(_, false) -> false;
get_windows_home(HomeDrive, HomePath) -> HomeDrive ++ HomePath.

