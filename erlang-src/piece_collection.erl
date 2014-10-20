-module(piece_collection).
-export([colorOf/1, sizeOf/1]).
-export([new/0, new/1, contains/2, equals/2]).
-export([empty/1, number/1, numberOf/2, numberAtLeast/2]).
-export([smallestOfColor/2, biggestSize/1, biggestSizeOf/2]).
-export([partitionByColor/2]).
-export([plus/2, plus/3, minus/2, minusAll/2]).
-export([isAdjacentTo/2]).
-export([toString/1, toComparableString/1, scan/1]).

-compile([{parse_transform, erl_aliases}]).
-module_alias({pc, piece_collection}).

-define(IsPiece(P), P==r1; P==r2; P==r3; P==y1; P==y2; P==y3; P==g1; P==g2; P==g3; P==b1; P==b2; P==b3).
-define(IsColor(Color), Color==red; Color==yellow; Color==green; Color==blue).
-define(IsSize(Size), Size==1; Size==2; Size==3).

s_tf(true) -> 1;
s_tf(false) -> 0.

s_count(List, Pred) -> s_count(List, Pred, 0).
s_count([], _Pred, Acc) -> Acc;
s_count([H|T], Pred, Acc) -> s_count(T, Pred, Acc + s_tf(Pred(H))).


%% A PieceCollection is represented as a sorted list of atoms (r1,y2, etc.)

colorOf(Piece) when Piece==r1; Piece==r2; Piece==r3 -> red;
colorOf(Piece) when Piece==y1; Piece==y2; Piece==y3 -> yellow;
colorOf(Piece) when Piece==g1; Piece==g2; Piece==g3 -> green;
colorOf(Piece) when Piece==b1; Piece==b2; Piece==b3 -> blue.

sizeOf(Piece) when Piece==r1; Piece==y1; Piece==g1; Piece==b1 -> 1;
sizeOf(Piece) when Piece==r2; Piece==y2; Piece==g2; Piece==b2 -> 2;
sizeOf(Piece) when Piece==r3; Piece==y3; Piece==g3; Piece==b3 -> 3.

s_piecesOfColor(red)    -> [r1,r2,r3];
s_piecesOfColor(yellow) -> [y1,y2,y3];
s_piecesOfColor(green)  -> [g1,g2,g3];
s_piecesOfColor(blue)   -> [b1,b2,b3].

s_piecesOfSize(1) -> [r1,y1,g1,b1];
s_piecesOfSize(2) -> [r2,y2,g2,b2];
s_piecesOfSize(3) -> [r3,y3,g3,b3].

new() -> [].
new(Piece) when ?IsPiece(Piece) -> [Piece].

contains(Pc1, Pc2) -> lists:all(fun(X) -> lists:member(X,Pc1) end, Pc2).

equals(Pc1, Pc2) -> pc:contains(Pc1, Pc2) andalso pc:contains(Pc2, Pc1).

empty([]) -> true;
empty(_) -> false.

number(Pc) -> length(Pc).
numberOf(Pc, Color) when ?IsColor(Color) ->
    Pieces = s_piecesOfColor(Color),
    Pred = fun(P) -> pc:contains(Pieces, P) end,
    s_count(Pc, Pred);
numberOf(Pc, Size) when ?IsSize(Size) ->
    Pieces = s_piecesOfSize(Size),
    Pred = fun(P) -> pc:contains(Pieces, P) end,
    s_count(Pc, Pred);
numberOf(Pc, Piece) ->
    Pred = fun(P) -> (P == Piece) end,
    s_count(Pc, Pred).

numberAtLeast(Pc, 1) -> number(Pc);
numberAtLeast(Pc, 2) -> number(Pc) - numberOf(Pc, 1);
numberAtLeast(Pc, 3) -> numberOf(Pc, 3).

smallestOfColor([], _Color) -> undefined;
smallestOfColor([H|T], Color) ->
    case colorOf(H) of
        Color -> H;
        _ -> smallestOfColor(T, Color)
    end.

s_bigFilter(Piece, undefined) -> sizeOf(Piece);
s_bigFilter(Piece, AccSize) -> max(sizeOf(Piece), AccSize).

biggestSize(Pc) -> lists:foldl(Pc, fun s_bigFilter/2, undefined).

biggestSizeOf(Pc, Color) ->
    Pieces = s_piecesOfColor(Color),
    PieceSizes = [ sizeOf(Piece) || Piece <- Pieces, pc:contains(Pc, [Piece]) ],
    lists:foldl(PieceSizes, fun erlang:max/2, undefined).

isAdjacentTo(Pc1, Pc2) ->
    Conflicts = [ pc:numberOf(Pc1, Size) > 0 andalso pc:numberOf(Pc2, Size) > 0 || Size <- [1,2,3] ],
    not lists:any(Conflicts).


plus(Pc, []) -> Pc;
plus(Pc, Pc2) when is_list(Pc2) -> lists:sort(Pc ++ Pc2);
plus(Pc, Piece) when is_atom(Piece) -> lists:sort([Piece|Pc]).
plus(Pc, Piece, Count) when is_atom(Piece) and is_integer(Count) -> plus(Pc, [Piece || _ <- lists:seq(1,Count)]).

minus(Pc, Piece) when is_atom(Piece) -> lists:delete(Pc, Piece);
minus(Pc, Pc2) when is_list(Pc2) ->
    Deleter = fun (Piece, List) -> lists:delete(List, Piece) end,
    lists:foldl(Deleter, Pc, Pc2).

minusAll(Pc, Color) when ?IsColor(Color) ->
    Pred = fun (Piece) -> (colorOf(Piece) == Color) end,
    lists:filter(Pred, Pc);
minusAll(Pc, Size) when ?IsSize(Size) ->
    Pred = fun (Piece) -> (sizeOf(Piece) == Size) end,
    lists:filter(Pred, Pc).

%% Returns a 2-tuple of PieceCollections: { pcOfColor, pcNotOfColor }.
partitionByColor(Pc, Color) when ?IsColor(Color) ->
    lists:partition(Pc, fun (Piece) -> colorOf(Piece) == Color end).

toString(Pc) ->
    Strings = [atom_to_list(Piece) || Piece <- Pc],
    strings:join(Strings, "").

%% Notice that this encoding scheme differs subtly from the C++ one.
toComparableString(Pc) -> toString(Pc).

s_scanPiece("r1") -> r1;
s_scanPiece("r2") -> r2;
s_scanPiece("r3") -> r3;
s_scanPiece("y1") -> y1;
s_scanPiece("y2") -> y2;
s_scanPiece("y3") -> y3;
s_scanPiece("g1") -> g1;
s_scanPiece("g2") -> g2;
s_scanPiece("g3") -> g3;
s_scanPiece("b1") -> b1;
s_scanPiece("b2") -> b2;
s_scanPiece("b3") -> b3;
s_scanPiece(_) -> throw(invalid).

%% Returns: a new PieceCollection
%% Throws: 'invalid'
%%
scan(Text, Acc) ->
    case Text of
        [] -> Acc;
        [Ch1,Ch2|Rest] ->
            scan(Rest, [s_scanPiece([Ch1,Ch2]) | Acc]);
        _ -> throw(invalid)
    end.
scan(Text) -> lists:sort(scan(Text, [])).
