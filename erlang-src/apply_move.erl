-module(apply_move).
-export([single/3, whole/3]).

-compile([{parse_transform, erl_aliases}]).
-module_alias({pc, piece_collection}).
-module_alias({ss, star_system}).
-module_alias({gs, game_state}).

-define(IsPiece(P), P==r1; P==r2; P==r3; P==y1; P==y2; P==y3; P==g1; P==g2; P==g3; P==b1; P==b2; P==b3).
-define(IsColor(Color), Color==red; Color==yellow; Color==green; Color==blue).
-define(IsSize(Size), Size==1; Size==2; Size==3).

%% Returns: the updated GameState
%% Throws: 'invalid_move'
%%
single(Gs, Attacker, Sa) ->
    Where = gs:system_named(Gs, sa:where(Sa)),
    (Where =/= undefined) orelse throw(invalid_move),
    Stash = gs:stash(Gs),
    NewGs =
        case sa:kind(Sa) of
            catastrophe ->
                Color = sa:color(Sa),
                ss:containsOverpopulation(Where, Color) orelse throw(invalid_move),
                { NewWhere, NewStash } = ss:performCatastrophe(Where, Color, Stash),
                gs:setStash(gs:updateStar(Gs, Where, NewWhere), NewStash);

            sacrifice ->
                Piece = sa:piece(Sa),
                ShipsPc = ss:shipsPc(Where, Attacker),
                (pc:numberOf(ShipsPc, Piece) > 0) orelse throw(invalid_move),
                NewWhere = ss:setShipsPc(Where, Attacker, pc:minus(ShipsPc, Piece)),
                NewStash = pc:plus(Stash, Piece),
                gs:setStash(gs:updateStar(Gs, Where, NewWhere), NewStash);

            capture ->
                Piece = sa:piece(Sa),
                ShipsAttacker = ss:shipsPc(Where, Attacker),
                ShipsDefender = ss:shipsPc(Where, 1-Attacker),
                (pc:numberOf(ShipsDefender, Piece) > 0) orelse throw(invalid_move),
                (pc:biggestSize(ShipsAttacker) >= pc:sizeOf(Piece)) orelse throw(invalid_move),
                NewWhere = ss:changeShipOwner(Where, Attacker, Piece),
                gs:setStash(gs:updateStar(Gs, Where, NewWhere), Stash);

            move ->
                Piece = sa:piece(Sa),
                ShipsAtWhere = ss:shipsPc(Where, Attacker),
                (pc:numberOf(ShipsAtWhere, Piece) > 0) orelse throw(invalid_move),
                Whither = gs:system_named(Gs, sa:whither(Sa)),
                (Whither =/= undefined) orelse throw(invalid_move),
                ss:isAdjacentTo(Where, Whither) orelse throw(invalid_move),
                ShipsAtWhither = ss:shipsPc(Whither, Attacker),
                NewWhere = ss:setShipsPc(Where, Attacker, pc:minus(ShipsAtWhere, Piece)),
                NewWhither = ss:setShipsPc(Whither, Attacker, pc:plus(ShipsAtWhither, Piece)),
                gs:updateStar(gs:updateStar(Gs, Where, NewWhere), Whither, NewWhither);

            move_create ->
                Piece = sa:piece(Sa),
                NewPiece = sa:newpiece(Sa),
                (pc:numberOf(Stash, NewPiece) > 0) orelse throw(invalid_move),
                ss:isAdjacentTo(ss:starPc(Where), pc:new(NewPiece)) orelse throw(invalid_move),
                ShipsAtWhere = ss:shipsPc(Where, Attacker),
                (pc:numberOf(ShipsAtWhere, Piece) > 0) orelse throw(invalid_move),
                (gs:system_named(Gs, sa:whither(Sa)) == undefined) orelse throw(invalid_move),
                Whither = ss:new(sa:whither(Sa), pc:new(NewPiece)),
                NewWhere = ss:setShipsPc(Where, Attacker, pc:minus(ShipsAtWhere, Piece)),
                NewWhither = ss:setShipsPc(Whither, Attacker, pc:new(Piece)),
                NewStash = pc:minus(Stash, Piece),
                gs:setStash(gs:addStar(gs:updateStar(Gs, Where, NewWhere), NewWhither), NewStash);

            build ->
                Piece = sa:piece(Sa),
                Color = pc:colorOf(Piece),
                ShipsPc = ss:shipsPc(Where, Attacker),
                (pc:numberOf(ShipsPc, Color) > 0) orelse throw(invalid_move),
                (Piece == pc:smallestOfColor(Stash, Color)) orelse throw(invalid_move),
                NewWhere = ss:setShipsPc(Where, Attacker, pc:plus(ShipsPc, Piece)),
                NewStash = pc:minus(Stash, Piece),
                gs:setStash(gs:updateStar(Gs, Where, NewWhere), NewStash);

            convert ->
                Piece = sa:piece(Sa),
                NewPiece = sa:newpiece(Sa),
                ShipsPc = ss:shipsPc(Where, Attacker),
                (pc:numberOf(ShipsPc, Piece) > 0) orelse throw(invalid_move),
                (pc:numberOf(Stash, NewPiece) > 0) orelse throw(invalid_move),
                (pc:sizeOf(NewPiece) == pc:sizeOf(Piece)) orelse throw(invalid_move),
                (pc:colorOf(NewPiece) =/= pc:colorOf(Piece)) orelse throw(invalid_move),
                NewWhere = ss:setShipsPc(Where, Attacker, pc:plus(pc:minus(ShipsPc, Piece), NewPiece)),
                NewStash = pc:plus(pc:minus(ShipsPc, NewPiece), Piece),
                gs:setStash(gs:updateStar(Gs, Where, NewWhere), NewStash)
        end,
    %% If an action has left this system with no ships in it,
    %% then return the star to the stash.
    gs:destroyEmptySystems(NewGs).

%% Returns: the updated GameState
%% Throws: 'invalid_move'
%%
whole(Gs, Attacker, Wm) ->
    %%
    %% A WholeMove is valid only if it consists of a possibly empty
    %% sequence of catastrophes, followed by an optional single sacrifice,
    %% followed by an appropriate number of regular actions, and finished
    %% with another possibly empty sequence of catastrophes.
    %%
    { _, _, _, NewGs, Attacker } = lists:foldl(fun s_helper/2, { precats, [], [], Gs, Attacker }, Wm),
    %%
    %% A losing move is not a legal move.
    %% A move that leaves your homeworld vulnerable to destruction-by-precatastrophe,
    %% without destroying your opponent's homeworld, is also illegal.
    %%
    (not gs:hasLost(NewGs, Attacker)) orelse throw(invalid_move),
    case gs:hasLost(NewGs, 1-Attacker) andalso ss:containsOverpopulation(gs:homeworldOf(NewGs, Attacker)) of
        true ->
            NewGsAfterCatastrophes = gs:performAllCatastrophes(NewGs),
            (not gs:hasLost(NewGsAfterCatastrophes, Attacker)) orelse throw(invalid_move);
        false -> ok
    end,
    NewGs.

s_helper(Sa, { precats, _, _, Gs, Attacker }) ->
    case sa:kind(Sa) of
        catastrophe -> { precat, [], [], apply_move:single(Gs, Attacker, Sa), Attacker };
        _ -> s_helper(Sa, { presac, [], [], Gs, Attacker })
    end;
s_helper(Sa, { presac, _, _, Gs, Attacker }) ->
    case sa:kind(Sa) of
        sacrifice ->
            Piece = sa:piece(Sa),
            { sac_actions, pc:colorOf(Piece), pc:sizeOf(Piece), apply_move:single(Gs, Attacker, Sa), Attacker };
        _ ->
            s_helper(Sa, { one_action, [], [], Gs, Attacker })
    end;
s_helper(Sa, { one_action, _, _, Gs, Attacker }) ->
    case sa:kind(Sa) of
        sacrifice -> throw(invalid_move);
        catastrophe -> s_helper(Sa, { postcat, [], [], Gs, Attacker });
        Action ->
            Where = gs:systemNamed(Gs, sa:where(Sa)),
            (Where =/= undefined) orelse throw(invalid_move),
            ss:playerHasAccessTo(Where, Attacker, sa:colorOfAction(Action)) orelse throw(invalid_move),
            { postcat, [], [], apply_move:single(Gs, Attacker, Sa), Attacker }
    end;
s_helper(Sa, { sac_actions, ActionColor, ActionsLeft, Gs, Attacker }) ->
    case sa:kind(Sa) of
        sacrifice -> throw(invalid_move);
        catastrophe -> s_helper(Sa, { postcat, [], [], Gs, Attacker });
        Action ->
            (sa:colorOfAction(Action) == ActionColor) orelse throw(invalid_move),
            (ActionsLeft > 0) orelse throw(invalid_move),
            { sac_actions, ActionColor, ActionsLeft-1, apply_move:single(Gs, Attacker, Sa), Attacker }
    end;
s_helper(Sa, { postcat, _, _, Gs, Attacker }) ->
    case sa:kind(Sa) of
        catastrophe -> { postcat, [], [], apply_move:single(Gs, Attacker, Sa), Attacker };
        _ -> throw(invalid_move)
    end.
