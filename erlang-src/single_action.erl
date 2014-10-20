-module(single_action).
-export([new/3,new/4,new/5]).
-export([kind/1, where/1, whither/1, color/1, piece/1, newpiece/1]).
-export([colorOfAction/1]).
-export([is_missing_pieces/1]).
-export([toString/1, scan/1]).

-compile([{parse_transform, erl_aliases}]).
-module_alias({pc, piece_collection}).
-module_alias({ss, star_system}).
-module_alias({gs, game_state}).

-define(IsPiece(P), P==r1; P==r2; P==r3; P==y1; P==y2; P==y3; P==g1; P==g2; P==g3; P==b1; P==b2; P==b3).
-define(IsColor(Color), Color==red; Color==yellow; Color==green; Color==blue).
-define(IsSize(Size), Size==1; Size==2; Size==3).

%% A SingleAction is represented as a record, with convenience getters and setters.

-record(sa, {
    kind,
    where,
    whither = undefined,
    color = undefined,
    piece = undefined,
    newpiece = undefined
}).

new(catastrophe, Color, Where) when ?IsColor(Color) -> #sa{kind=catastrophe, color=Color, where=Where};
new(sacrifice, Piece, Where) when ?IsPiece(Piece) -> #sa{kind=sacrifice, piece=Piece, where=Where};
new(capture, Piece, Where) when ?IsPiece(Piece) -> #sa{kind=capture, piece=Piece, where=Where};
new(build, Piece, Where) when ?IsPiece(Piece) -> #sa{kind=build, piece=Piece, where=Where}.

new(move, Piece, Where, Whither) when ?IsPiece(Piece) -> #sa{kind=move, piece=Piece, where=Where, whither=Whither};
new(convert, Piece, Where, NewPiece) when ?IsPiece(Piece) andalso ?IsPiece(NewPiece) ->
    #sa{kind=convert, piece=Piece, where=Where, newpiece=NewPiece}.

new(move_create, Piece, Where, Whither, NewStarPiece) when ?IsPiece(Piece) andalso ?IsPiece(NewStarPiece) ->
    #sa{kind=move_create, piece=Piece, where=Where, whither=Whither, newpiece=NewStarPiece}.

kind(Sa) -> Sa#sa.kind.
where(Sa) -> Sa#sa.where.
whither(Sa) -> Sa#sa.whither.
color(Sa) -> Sa#sa.color.
piece(Sa) -> Sa#sa.piece.
newpiece(Sa) -> Sa#sa.newpiece.

colorOfAction(catastrophe) -> undefined;
colorOfAction(sacrifice) -> undefined;
colorOfAction(capture) -> red;
colorOfAction(move) -> yellow;
colorOfAction(move_create) -> yellow;
colorOfAction(build) -> green;
colorOfAction(convert) -> blue.

is_missing_pieces(_SingleAction) -> throw(unimplemented).

toString(#sa{kind=catastrophe, color=Color, where=Where}) -> "catastrophe " ++ atom_to_list(Color) ++ " at " ++ Where;
toString(#sa{kind=sacrifice, piece=Piece, where=Where}) -> "sacrifice " ++ atom_to_list(Piece) ++ " at " ++ Where;
toString(#sa{kind=capture, piece=Piece, where=Where}) -> "capture " ++ atom_to_list(Piece) ++ " at " ++ Where;
toString(#sa{kind=build, piece=Piece, where=Where}) -> "build " ++ atom_to_list(Piece) ++ " at " ++ Where;
toString(#sa{kind=move, piece=Piece, where=Where, whither=Whither}) -> "move " ++ atom_to_list(Piece) ++ " from " ++ Where ++ " to " ++ Whither;
toString(#sa{kind=convert, piece=Piece, where=Where, newpiece=NewPiece}) -> "convert " ++ atom_to_list(Piece) ++ " at " ++ Where ++ " to " ++ NewPiece;
toString(#sa{kind=move_create, piece=Piece, where=Where, whither=Whither, newpiece=NewPiece}) -> "move " ++ atom_to_list(Piece) ++ " from " ++ Where ++ " to " ++ Whither ++ " (" ++ NewPiece ++ ")".

scan(_Text) -> throw(unimplemented).
