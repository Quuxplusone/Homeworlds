-module(game_state).
-export([newGame/0]).
-export([stars/1, stash/1]).
-export([setStars/2, setStash/2, updateStar/3]).
-export([systemNamed/2, homeworldOf/2]).
-export([containsOverpopulation/1, performAllCatastrophes/1, destroyEmptySystems/1]).
-export([removeSystem/2, removeSystemNamed/2]).
-export([hasLost/2, gameIsOver/1]).
-export([mirror/1]).
-export([toString/1, toComparableString/1, scan/1]).

-compile([{parse_transform, erl_aliases}]).
-module_alias({pc, piece_collection}).
-module_alias({ss, star_system}).
-module_alias({gs, game_state}).

-define(IsPiece(P), P==r1; P==r2; P==r3; P==y1; P==y2; P==y3; P==g1; P==g2; P==g3; P==b1; P==b2; P==b3).
-define(IsColor(Color), Color==red; Color==yellow; Color==green; Color==blue).
-define(IsSize(Size), Size==1; Size==2; Size==3).

%% A GameState is represented as a record, with convenience getters and setters.
%% { StarsList, StashPc }.

-record(gs, {
    stars = [],
    stash = s_fullstash()
}).

newGame() -> #gs{}.

stars(Gs) -> Gs#gs.stars.
setStars(Gs, Stars) -> Gs#gs{stars=Stars}.
stash(Gs) -> Gs#gs.stash.
setStash(Gs, Stash) -> Gs#gs{stash=Stash}.

updateStar(Gs, OldStar, NewStar) ->
    gs:setStars(Gs, [case X of OldStar -> NewStar; _ -> X end || X <- gs:stars(Gs)]).

s_fullstash() ->
    OneSet = pc:plus(pc:piecesOfSize(1), pc:piecesOfSize(2), pc:piecesOfSize(3)),
    pc:plus(OneSet, OneSet, OneSet).

%% Returns: a StarSystem, or 'undefined'
%% Throws for debugging purposes only: 'two_stars_with_same_name'
%%
systemNamed(Gs, StarName) ->
    Stars = gs:stars(Gs),
    case [Ss || Ss <- Stars, ss:name(Ss) == StarName] of
        [] -> undefined;
        [Ss] -> Ss;
        _ -> throw(two_stars_with_same_name)
    end.

%% Returns: a StarSystem, or 'undefined'
%% Throws for debugging purposes only: 'player_has_two_homeworlds'
%%
homeworldOf(Gs, PlayerNumber) ->
    Stars = gs:stars(Gs),
    case [Ss || Ss <- Stars, ss:homeworldOf(Ss) == PlayerNumber] of
        [] -> undefined;
        [Ss] -> Ss;
        _ -> throw(player_has_two_homeworlds)
    end.

containsOverpopulation(Gs) ->
    Stars = gs:stars(Gs),
    lists:any(fun ss:containsOverpopulation/1, Stars).

s_performCatIfPossible(Color, {Ss, Stash}) ->
    case ss:containsOverpopulation(Ss, Color) of
        true -> ss:performCatastrophe(Ss, Color, Stash);
        false -> { Ss, Stash }
    end.

s_performAllCatsOnStar(Ss, Stash) ->
    s_performCatIfPossible(red,
    s_performCatIfPossible(yellow,
    s_performCatIfPossible(green,
    s_performCatIfPossible(blue, { Ss, Stash })))).

performAllCatastrophes(Gs) ->
    {NewStars, NewStash} = lists:mapfoldl(fun s_performAllCatsOnStar/2, gs:stash(Gs), gs:stars(Gs)),
    NewerStars = [Ss || Ss <- NewStars, not pc:empty(ss:starPc(Ss))],
    Gs#gs{stars = NewerStars, stash = NewStash}.

s_mapEmptySystemToSupernova(Ss, Stash) ->
    case pc:empty(ss:starPc(Ss)) orelse ss:hasNoShips(Ss) of
        true -> { supernova, pc:plus(Stash, ss:pieceCollection(Ss)) };
        false -> { Ss, Stash }
    end.

%% Remove from the GameState any systems where either the star itself is gone (empty),
%% or all the ships at that star are gone.
%%
destroyEmptySystems(Gs) ->
    {NewStars, NewStash} = lists:mapfoldl(fun s_mapEmptySystemToSupernova/2, gs:stash(Gs), gs:stars(Gs)),
    NewerStars = [Ss || Ss <- NewStars, Ss =/= supernova],
    Gs#gs{stars = NewerStars, stash = NewStash}.

removeSystem(Gs, Ss_) ->
    gs:setStars(Gs, [Ss || Ss <- gs:stars(Gs), Ss =/= Ss_]).

removeSystemNamed(Gs, StarName) ->
    gs:setStars(Gs, [Ss || Ss <- gs:stars(Gs), ss:name(Ss) =/= StarName]).

hasLost(Gs, PlayerNumber) ->
    case gs:homeworldOf(Gs, PlayerNumber) of
        undefined -> true;
        Hw -> pc:empty(ss:shipsPc(Hw, PlayerNumber))
    end.

mirror(Gs) ->
    NewStars = lists:map(fun ss:mirror/1, gs:stars(gs)),
    gs:setStars(Gs, NewStars).

gameIsOver(Gs) -> hasLost(Gs, 0) orelse hasLost(Gs, 1).

toString(Gs) ->
    Strings = [ ss:toString(Ss) || Ss <- gs:stars(Gs) ],
    string:join(Strings, "\n").

%% Notice that this encoding scheme differs subtly from the C++ one.
toComparableString(Gs) ->
    %% Put the homeworlds first, because they are "special" regardless of
    %% their position in the galaxy or their names.
    Hw0 =
        case gs:homeworldOf(Gs, 0) of
            undefined -> "";
            Ss0 -> ss:toComparableString(Ss0)
        end,
    Hw1 =
        case gs:homeworldOf(Gs, 1) of
            undefined -> "";
            Ss1 -> ss:toComparableString(Ss1)
        end,
    %% For non-homeworld stars, the only thing that matters is the piece
    %% distribution of the star and ships; name and position don't matter.
    %% So use star_system:toComparableString() to throw out the name,
    %% and sort the results to get rid of position information. */
    Others = [ ss:toComparableString(Ss) || Ss <- gs:stars(Gs), ss:homeworldOf(Ss) == -1 ],
    Strings = [Hw0 | [Hw1 | lists:sort(Others)]],
    string:join(Strings, "!").


%% Read a sequence of lines from the given file, where each line matches
%% the representation expected by star_system:scan()... except that blank
%% lines are ignored, and lines ending with "# comment text" are trimmed
%% before processing.
%%   Because our input is line-buffered, we read a bunch of matching lines
%% and then we read one non-matching line before returning. We don't want
%% to just throw away that line, so we return it as a string.
%%   If we can parse everything in the file, then we return "". This is
%% not ambiguous, because we consider a blank line (or a line consisting
%% only of a comment) to be parseable, and thus we will never return ""
%% in any other circumstance.
%%
%% Returns a tuple { Gs, UnparsedLine }.
%% Never throws.
%%
s_scan(Gs, Device) ->
    case io:get_line(Device, "") of
        eof -> { Gs, "" };
        Line ->
            case string:strip(Line) of
                [] -> s_scan(Gs, Device);
                [$#|_] -> s_scan(Gs, Device);
                StrippedLine ->
                    try ss:scan(StrippedLine) of
                        Ss ->
                            NewStash = pc:minus(gs:stash(Gs), ss:pieceCollection(Ss)),
                            NewStars = [Ss | gs:stars(Gs)],
                            NewGs = Gs#gs{stars=NewStars, stash=NewStash},
                            s_scan(NewGs, Device)
                    catch
                        invalid -> { Gs, Line }
                    end
            end
    end.

scan(Device) ->
    s_scan( gs:newGame(), Device ).
