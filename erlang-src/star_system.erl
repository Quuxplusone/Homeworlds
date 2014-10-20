-module(star_system).
-export([new/0, new/1, new/2]).
-export([name/1, starPc/1, shipsPc/2, homeworldOf/1]).
-export([setName/2, setStarPc/2, setShipsPc/3, setHomeworldOf/2]).
-export([clear/1, changeShipOwner/3]).
-export([isValidName/1, makeRandomName/1]).
-export([numberOfShips/1, hasNoShips/1, number/1, numberOf/2]).
-export([containsOverpopulation/1, containsOverpopulation/2, canCatastropheStar/1, performCatastrophe/3]).
-export([playerHasAccessTo/3]).
-export([isAdjacentTo/2, pieceCollection/1]).
-export([mirror/1]).
-export([toString/1, toComparableString/1, scan/1]).

-compile([{parse_transform, erl_aliases}]).
-module_alias({pc, piece_collection}).
-module_alias({ss, star_system}).

-define(IsPiece(P), P==r1; P==r2; P==r3; P==y1; P==y2; P==y3; P==g1; P==g2; P==g3; P==b1; P==b2; P==b3).
-define(IsColor(Color), Color==red; Color==yellow; Color==green; Color==blue).
-define(IsSize(Size), Size==1; Size==2; Size==3).

%% A StarSystem is represented as a record, with convenience getters and setters.
%% { Name, PcStar, PcShips0, PcShips1, HomeworldOf }.

-record(ss, {
    name = "",
    star = [],
    ships0 = [],
    ships1 = [],
    homeworldOf = -1
}).

new() -> #ss{}.
new(Name) -> #ss{name=Name}.
new(Name, StarPc) -> #ss{name=Name, star=StarPc}.

name(Ss) -> Ss#ss.name.
setName(Ss, Name) -> Ss#ss{name=Name}.
starPc(Ss) -> Ss#ss.star.
setStarPc(Ss, Pc) -> Ss#ss{star=Pc}.
shipsPc(Ss, 0) -> Ss#ss.ships0;
shipsPc(Ss, 1) -> Ss#ss.ships1.
setShipsPc(Ss, 0, Pc) -> Ss#ss{ships0=Pc};
setShipsPc(Ss, 1, Pc) -> Ss#ss{ships1=Pc}.
homeworldOf(Ss) -> Ss#ss.homeworldOf.
setHomeworldOf(Ss, PlayerNumber) -> Ss#ss{homeworldOf=PlayerNumber}.

changeShipOwner(Ss, Attacker, Piece) ->
    Defender = 1-Attacker,
    ShipsAttacker = pc:plus(ss:shipsPc(Ss, Attacker), Piece),
    ShipsDefender = pc:minus(ss:shipsPc(Ss, Defender), Piece),
    Ships0 = case Attacker of 0 -> ShipsAttacker; 1 -> ShipsDefender end,
    Ships1 = case Attacker of 1 -> ShipsAttacker; 0 -> ShipsDefender end,
    Ss#ss{ships0=Ships0, ships1=Ships1}.

clear(Ss) -> Ss#ss{star=pc:new(), ships0=pc:new(), ships1=pc:new()}.

s_isValidNameCharacter(Ch) when ($A =< Ch andalso Ch =< $Z);
                                ($a =< Ch andalso Ch =< $z);
                                ($0 =< Ch andalso Ch =< $9) -> true;
s_isValidNameCharacter(_) -> false.
isValidName(Text) -> (Text =/= "") andalso lists:all(fun s_isValidNameCharacter/1, Text).

makeRandomName(_Gs) ->
    throw(unimplemented).

numberOfShips(Ss) -> shipsPc(Ss, 0) + shipsPc(Ss, 1).
hasNoShips(Ss) -> shipsPc(Ss, 0)==0 andalso shipsPc(Ss, 1)==0.

number(Ss) ->
    numberOfShips(Ss) + pc:number(starPc(Ss)).

numberOf(Ss, Color) when ?IsColor(Color) ->
    pc:numberOf(shipsPc(Ss, 0), Color) + pc:numberOf(shipsPc(Ss, 1), Color) + pc:numberOf(starPc(Ss), Color).

containsOverpopulation(Ss, Color) when ?IsColor(Color) -> (ss:numberOf(Ss, Color) >= 4).
containsOverpopulation(Ss) ->
    ss:containsOverpopulation(Ss, red) orelse ss:containsOverpopulation(Ss, yellow) orelse
    ss:containsOverpopulation(Ss, green) orelse ss:containsOverpopulation(Ss, blue).

canCatastropheStar(Ss) ->
    CanCatastropheColor = fun (Piece) -> ss:containsOverpopulation(Ss, pc:colorOf(Piece)) end,
    lists:all(CanCatastropheColor, ss:starPc(Ss)).

%% Returns { NewSs, NewPcStash }.
performCatastrophe(Ss, Color, PcStash) when ?IsColor(Color) ->
    %% Cull the star's pieces of color "Color".
    { ToStash1, NewStar } = pc:partitionByColor(starPc(Ss), Color),
    case pc:empty(NewStar) of
        true ->
            %% If the star has been destroyed, then destroy all the ships here.
            NewStash = pc:plus(PcStash, ss:pieceCollection(Ss)),
            { ss:clear(Ss), NewStash };
        false ->
            %% Cull the ships of color "Color".
            { ToStash2, NewShips0 } = pc:partitionByColor(shipsPc(Ss, 0), Color),
            { ToStash3, NewShips1 } = pc:partitionByColor(shipsPc(Ss, 1), Color),
            NewStash = pc:plus(PcStash, ToStash1, ToStash2, ToStash3),
            NewSs = Ss#ss{star=NewStar, ships0=NewShips0, ships1=NewShips1},
            case ss:hasNoShips(NewSs) of
                true ->
                    %% If the last ship has been destroyed, then destroy the star.
                    NewerStash = pc:plus(NewStash, ss:pieceCollection(NewSs)),
                    { ss:clear(NewSs), NewerStash };
                false ->
                    { NewSs, NewStash }
            end
    end.

playerHasAccessTo(Ss, Player, Color) when ?IsColor(Color) ->
    pc:numberOf(starPc(Ss), Color) > 0 orelse pc:numberOf(shipsPc(Ss, Player), Color) > 0.

isAdjacentTo(Ss1, Ss2) -> pc:isAdjacentTo(starPc(Ss1), starPc(Ss2)).

pieceCollection(Ss) -> pc:plus(starPc(Ss), shipsPc(Ss, 0), shipsPc(Ss, 1)).

mirror(Ss) ->
    S1 = ss:setShipsPc(Ss, 0, ss:shipsPc(Ss, 1)),
    S2 = ss:setShipsPc(S1, 1, ss:shipsPc(Ss, 0)),
    ss:setHomeworldOf(S2,
        case ss:homeworldOf(Ss) of
            0 -> 1;
            1 -> 0;
            -1 -> -1
        end).

toString(Ss) ->
    Strings = [
        ss:name(Ss),
        case ss:name(Ss) of
            "" -> "";
            _ -> " "
        end,
        "(",
        pc:toString(ss:starPc(Ss)),
        case ss:homeworldOf(Ss) of
            0 -> ", 0) ";
            1 -> ", 1) ";
            -1 -> ") "
        end,
        pc:toString(ss:shipsPc(Ss, 0)),
        "-",
        pc:toString(ss:shipsPc(Ss, 1))
    ],
    strings:join(Strings, "").

%% Notice that this encoding scheme differs subtly from the C++ one.
toComparableString(Ss) ->
    Strings = [
        pc:toComparableString(ss:starPc(Ss)),
        "/",
        pc:toComparableString(ss:shipsPc(Ss, 0)),
        "-",
        pc:toComparableString(ss:shipsPc(Ss, 1))
    ],
    string:join(Strings, "").

s_isspace(Ch) when (Ch == 32); (Ch == 10); (Ch == 13); (Ch == 9) -> true;
s_isspace(_) -> false.

%% Masses of impenetrable Erlang!
%% Returns: a new StarSystem
%% Throws: 'invalid'
%%
scan(Text) ->
    UnwhitedText = [Ch || Ch <- Text, not s_isspace(Ch)],
    [StarName_, Parenthesized_, Ships_] =
        case string:tokens(UnwhitedText, "()") of
            [Parenthesized, Ships] ->
                ExpectedText = string:join(["(",Parenthesized,")",Ships], ""),
                case UnwhitedText == ExpectedText of
                    true -> ok;
                    false -> throw(invalid)
                end,
                ["", Parenthesized, Ships];
            [StarName, Parenthesized, Ships] ->
                ExpectedText = string:join([StarName,"(",Parenthesized,")",Ships], ""),
                case UnwhitedText == ExpectedText of
                    true -> ok;
                    false -> throw(invalid)
                end,
                [StarName, Parenthesized, Ships];
            _ -> throw(invalid)
        end,
    [Homeworld_, StarPc_] =
        case string:tokens(Parenthesized_, ",") of
            [StarPc] ->
                (Parenthesized_ == StarPc) orelse throw(invalid),
                [-1, StarPc];
            ["0", StarPc] ->
                (Parenthesized_ == ("0," ++ StarPc)) orelse throw(invalid),
                [0, StarPc];
            ["1", StarPc] ->
                (Parenthesized_ == ("1," ++ StarPc)) orelse throw(invalid),
                [1, StarPc];
            _ -> throw(invalid)
        end,
    [Ships0_,Ships1_] =
        case string:tokens(Ships_, "-") of
            [Ships0, Ships1] -> [Ships0, Ships1];
            [Sh] ->
                case Ships_ of
                    [$-|_] ->
                        [pc:new(), Sh];
                    _ ->
                        lists:last(Ships_) == $- orelse throw(invalid),
                        [Sh, pc:new()]
                end;
            _ -> throw(invalid)
        end,
    ss:isValidName(StarName_) orelse throw(invalid),
    #ss{
        name = StarName_,
        star = pc:scan(StarPc_),
        ships0 = pc:scan(Ships0_),
        ships1 = pc:scan(Ships1_),
        homeworldOf = Homeworld_
    }.
