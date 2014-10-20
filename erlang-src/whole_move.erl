-module(whole_move).
-export([new/0]).
-export([isPass/1, plus/2]).
-export([toString/1, scan/1]).

%% A WholeMove is represented as a list of SingleActions.

new() -> [].

isPass([]) -> true;
isPass(_) -> false.

plus(Wm, Sa) -> Wm ++ [Sa].

toString([]) -> "pass";
toString(Wm) ->
    Strings = [ single_action:toString(Sa) || Sa <- Wm ],
    string:join(Strings, "; ").

scan(_Text) -> throw(unimplemented).
