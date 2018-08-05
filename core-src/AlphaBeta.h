#pragma once

#include <stddef.h>
#include <assert.h>
#include <queue>
#include <vector>

template<typename State        // a state of the world, not necessarily including whose turn it is
        ,typename Move         // an indication of how to get from one state to another state
        ,typename Value        // a scalar "goodness" measure (e.g., "int" or "double")
         >
class AlphaBeta {
    // Given a State, return an approximation of its Value to the defender.
    // (Its value to the attacker is just the negation of this value.)
    // A State which the defender is happy to defend will have a high Value.
    // Given a move M between states S1 and S2, one would expect M to be
    // chosen if findattacker(S1)!=findattacker(S2) and evaluate(S2) > 0
    // (the usual case), or if findattacker(S1)==findattacker(S2) and
    // (-evaluate(S2)) > 0 --- that is, iff evaluate2(S1,S2) > 0.
    typedef Value (*Evaluator)(const State &st);
    // Given a State, apply the given Move to produce a new State.
    typedef void (*MoveApplier)(State &st, const Move &move);
    // Given a State, find all moves available to the attacker and add them
    // to the given vector (which will be initially empty).
    typedef void (*MoveFinder)(const State &st, std::vector<Move> &allmoves);
    // Given a State, tell me which player is the next to move.
    // Generally the State will contain a field that flips between 0 and 1
    // each time applymove() is called, and findattacker() will just return
    // that field.
    typedef int (*AttackerFinder)(const State &st);

    const Evaluator evaluate;
    const MoveApplier applymove;
    const MoveFinder findmoves;
    const AttackerFinder findattacker;

    int finddefender(const State &st) {
        return 1-findattacker(st);
    }
    // Return a high Value if s1's attacker wants to move to s2.
    Value evaluate2(const State &s1, const State &s2) {
        if (findattacker(s1) != findattacker(s2)) {
            return evaluate(s2);
        }
        return -evaluate(s2);
    }

  public:
    AlphaBeta(Evaluator ev, MoveApplier app, MoveFinder fm, AttackerFinder fa):
            evaluate(ev), applymove(app), findmoves(fm), findattacker(fa) { }

    /* Given a State, return the best possible move for the attacker (looking
     * "ply" plies deep).  If the attacker has no legal moves (not even
     * "pass"), then return false; else return true.
     * The state after applying this move will have a chain of "best plays"
     * leading away from it down the tree; return the expected value of the
     * endpoint of that chain to the current defender. That is, if we find
     * a winning move for "attacker", we'll set "bestvalue" to a negative value.
     */
    bool depth_first(const State &st, int ply,
                     Move &bestmove, Value &bestvalue);

    /* Same deal as above, but using alpha-beta pruning to speed up the search
     * for large values of "ply". On the initial call, "alpha" should be set
     * to the most negative Value and "beta" to the most positive Value.
     *   To search for a winning move (looking "ply" plies deep), start "alpha"
     * at something just a tiny bit less than the value of a won game, and "beta"
     * at the most positive Value.
     *   Alternatively, to search for the first move that simply doesn't lose
     * the game (looking "ply" plies deep), start "beta" at something just a
     * tiny bit greater than the value of a lost game, and "alpha" at the most
     * negative Value.
     */
    bool depth_first_alpha_beta(const State &st, int ply,
                                Move &bestmove, Value &bestvalue,
                                Value alpha, Value beta);

    /* Given a State, return the best possible move using alpha-beta pruning,
     * as above. However, rather than searching depth-first, we'll search
     * breadth-first. This means that instead of recursively calling
     * breadth_first_alpha_beta(), we'll push an "action record" of all
     * the call's parameters onto an action queue, and then suspend the
     * call until that "action record" reaches the front of the queue.
     * At that point we'll continue processing that state.
     *   In the depth-first search, after we'd recursed on all of a given
     * state's children, we would take the maximum of all their outputs.
     * In this breadth-first search, after we've pushed action records for
     * all of a given state's children, we will push another action record
     * indicating that it's time to "return a value" from this state.
     *   Once we've inserted "maxnodes" states into our queue,
     * we'll bottom out by using this->evaluate() on all the remaining
     * states in the queue and processing the rest of the "value-returning"
     * action records.
     */
    bool breadth_first(const State &st, int maxnodes,
                       Move &bestmove, Value &bestvalue);
  private:
    enum BFR_type_t { RECURSE, RETURN };
    struct BFRecord {
        BFR_type_t type;
        BFRecord *parent;
        Move move;  // What move did we use to get here?
                    // record->move may become record->parent->bestmove.
        /* If type==RECURSE: */
        State st;   // starting state
        /* If type==RETURN: */
        int unreported_children;
        int attacker;
        bool hasbestvalue;
        Value bestvalue;
        Move bestmove;  // The best move from this point; its value is bestvalue

        BFRecord(BFR_type_t t, const State &s, const Move &m, BFRecord *p):
            type(t), parent(p), move(m), st(s)
            { assert(t == RECURSE); assert(!p || p->type == RETURN); }
        BFRecord(BFR_type_t t, const Move &m, int u, int a, BFRecord *p):
            type(t), parent(p), move(m), unreported_children(u), attacker(a), hasbestvalue(false)
            { assert(t == RETURN); assert(!p || p->type == RETURN); }
#if 1
        void print() const {
            printf("%s, parent=%p, move=%c, ", type==RETURN?"RETURN":"RECURSE", (void*)parent, move.letter);
            if (type == RECURSE) {
                printf("st=??\n");
            } else {
                printf("uch=%d, attacker=%d", unreported_children, attacker);
                if (hasbestvalue) {
                    printf(": bestmove is %c (value=%d)\n", bestmove.letter, bestvalue);
                } else {
                    printf(": has no bestmove yet\n", parent);
                }
            }
        }
#endif /* 1 */
    };
};


template <typename State, typename Move, typename Value>
bool AlphaBeta<State,Move,Value>::depth_first(const State &st, int ply,
                                              Move &bestmove, Value &bestvalue)
{
    assert(ply >= 0);
    /* This is the base case. Returning false from depth_first() basically
     * means "game over; stop and evaluate() this position heuristically".
     * This is exactly correct in the case where the game is actually over
     * (see below), and coincidentally it turns out to be the right thing
     * to do when we hit the ply limit as well. */
    if (ply == 0) {
        return false;
    }
    const int attacker = this->findattacker(st);
    std::vector<Move> allmoves;
    this->findmoves(st, allmoves);
    /* If the attacker has no possible moves (not even a move corresponding
     * to "pass", if this game allows players to pass), then the game is
     * over. Return false, meaning "game over", as explained above. */
    if (allmoves.empty()) {
        return false;
    }
    /* Otherwise, the attacker has some possible moves, and we're going to
     * look more than one ply deep.  The best move in these cases is the move
     * which the attacker is happiest to defend --- i.e., the move where if
     * we run AlphaBeta.depth_first() on it from the opponent's point of view,
     * we get back a very positive number.
     */
    Value highestvalue(0);
    int highestidx = -1;
    for (int i=0; i < (int)allmoves.size(); ++i) {
        State newstate = st;
        this->applymove(newstate, allmoves[i]);
        Move dbestmove; // unused
        /* "dhighestvalue" will receive the value of "newstate" from the
         * point of view of "newattacker" (who is trying to maximize that
         * value). If newattacker != attacker, as in chess or checkers,
         * then the value of this move to "attacker" will be the negation
         * of "dhighestvalue". But if newattacker == attacker, then the value
         * of this move to the attacker is obviously dhighestvalue itself. */
        Value dhighestvalue;
        Value value_to_me;
        int newattacker = findattacker(newstate);
        /* Generally, we'd expect that newattacker != attacker. */
        bool foundmove = this->depth_first(newstate, ply-1, dbestmove, dhighestvalue);
        if (!foundmove) {
            /* If the defender has no moves left, then the game is definitely
             * over. We must evaluate this position to see how happy we are
             * with it. If we are very happy to defend it, then it is a
             * position in which we have won the game. If we are very unhappy
             * with it, then it is a position in which we have lost the game
             * by making this move.
             */
            value_to_me = this->evaluate2(st, newstate);
        } else {
            value_to_me = (newattacker != attacker) ? -dhighestvalue : dhighestvalue;
        }
        if (highestidx == -1 || value_to_me > highestvalue) {
            highestidx = i;
            highestvalue = value_to_me;
        }
    }
    assert(highestidx != -1);
    bestmove = allmoves[highestidx];
    bestvalue = highestvalue;
    return true;
}


/* This version is equivalent to depth_first(), but it takes two extra
 * parameters "alpha" and "beta". "Alpha" is the smallest value the attacker
 * can get if he plays optimally to maximize his score; it starts at -inf
 * and gets higher.
 * "Beta" is the biggest value the attacker can get if the defender plays
 * optimally to counter him; it starts at +inf and gets lower.
 * "Alpha" and "beta" swap places every half-move down the tree.
 */
template <typename State, typename Move, typename Value>
bool AlphaBeta<State,Move,Value>::depth_first_alpha_beta(
                                              const State &st, int ply,
                                              Move &bestmove, Value &bestvalue,
                                              Value alpha, Value beta)
{
    assert(ply >= 0);
    /* This is the base case. Returning false from depth_first() basically
     * means "game over; stop and evaluate() this position heuristically".
     * This is exactly correct in the case where the game is actually over
     * (see below), and coincidentally it turns out to be the right thing
     * to do when we hit the ply limit as well. */
    if (ply == 0) {
        return false;
    }
    const int attacker = this->findattacker(st);
    std::vector<Move> allmoves;
    this->findmoves(st, allmoves);
    /* If the attacker has no possible moves (not even a move corresponding
     * to "pass", if this game allows players to pass), then the game is
     * over. Return false, meaning "game over", as explained above. */
    if (allmoves.empty()) {
        return false;
    }
    /* Otherwise, the attacker has some possible moves, and we're going to
     * look more than one ply deep.  The best move in these cases is the move
     * which the attacker is happiest to defend --- i.e., the move where if
     * we run AlphaBeta.depth_first() on it from the opponent's point of view,
     * we get back a very positive number.
     */
    Value highestvalue;
    int highestidx = -1;
    bool bail_out_early = false;
    for (int i=0; i < (int)allmoves.size(); ++i) {
        if (bail_out_early) continue;
        State newstate = st;
        this->applymove(newstate, allmoves[i]);
        Move dbestmove; // unused
        /* "dhighestvalue" will receive the value of "newstate" from the
         * point of view of "newattacker" (who is trying to maximize that
         * value). If newattacker != attacker, as in chess or checkers,
         * then the value of this move to "attacker" will be the negation
         * of "dhighestvalue". But if newattacker == attacker, then the value
         * of this move to the attacker is obviously dhighestvalue itself. */
        Value dhighestvalue;
        Value value_to_me;
        int newattacker = findattacker(newstate);
        /* Generally, we'd expect that newattacker != attacker. */
        bool foundmove = this->depth_first(newstate, ply-1, dbestmove, dhighestvalue);
        if (!foundmove) {
            /* If the defender has no moves left, then the game is definitely
             * over. We must evaluate this position to see how happy we are
             * with it. If we are very happy to defend it, then it is a
             * position in which we have won the game. If we are very unhappy
             * with it, then it is a position in which we have lost the game
             * by making this move.
             */
            value_to_me = this->evaluate2(st, newstate);
        } else {
            value_to_me = (newattacker != attacker) ? -dhighestvalue : dhighestvalue;
        }
        if (highestidx == -1 || value_to_me > highestvalue) {
            highestidx = i;
            highestvalue = value_to_me;
            /* We just found a move we are very happy to defend.
             * Since we now know that we can do at least this well,
             * we should set alpha = max(alpha, value_to_me). */
            if (value_to_me > alpha) {
                alpha = value_to_me;
                if (value_to_me >= beta) {
                    /* We know we can't possibly do better than beta,
                     * so if we've found a move worth at least beta
                     * then we can stop looking. */
                    bail_out_early = true;
                }
            }
        }
    }
    assert(highestidx != -1);
    bestmove = allmoves[highestidx];
    bestvalue = highestvalue;
    return true;
}


template <typename State, typename Move, typename Value>
bool AlphaBeta<State,Move,Value>::breadth_first(const State &st, int maxnodes,
                                                Move &bestmove, Value &bestvalue)
{
    assert(maxnodes >= 0);
    if (maxnodes == 0) {
        return false;
    }

    std::queue<BFRecord *> Q;

    /* The queue starts out with all the moves from this state. */
    std::vector<Move> allmoves;
    this->findmoves(st, allmoves);
    /* If the attacker has no moves, return false. */
    if (allmoves.empty()) {
        return false;
    }

    /* Otherwise, initialize the queue by pushing action records for each of
     * the attacker's possible moves. Link them all to "return_record", which
     * has a parent of NULL --- that's how we'll know when we hit the top of
     * the game tree again. */
    int insertednodes = 0;
    BFRecord *top_level_return_record = new BFRecord(RETURN, Move(), (int)allmoves.size(), this->findattacker(st), NULL);
    for (int i=0; i < (int)allmoves.size(); ++i) {
        Q.push(new BFRecord(RECURSE, st, allmoves[i], top_level_return_record));
        insertednodes += 1;
        if (insertednodes == maxnodes) {
            top_level_return_record->unreported_children = i+1;
            break;
        }
    }
    Q.push(top_level_return_record);

    /* The queue is now initialized. */

    for ( ; !Q.empty(); Q.pop()) {
        assert(insertednodes <= maxnodes);
        BFRecord *record = Q.front();

        if (record->type == RETURN) {
            assert(record->unreported_children >= 0);
            if (record->unreported_children > 0) {
                /* We can't process this record yet; not all of its children
                 * have reported in. Throw it back into the queue. */
                Q.push(record);
                continue;
            } else {
                assert(record->unreported_children == 0);
                if (record->parent == NULL) {
                    /* This is the very first record pushed on the queue,
                     * the one that holds this function's actual return
                     * values. Break out of the loop at this point. */
                    assert(record == top_level_return_record);
                    assert(record->hasbestvalue);
                    bestmove = record->bestmove;
                    bestvalue = record->bestvalue;
                    /* Note that "record" is still in the queue. */
                    goto delete_any_remaining_queue_elements;
                }
                /* Otherwise, record.bestvalue holds the Value of record.move
                 * (a move which yields record.state), from the point of view
                 * of the attacker who just made that move. We need to
                 * propagate that bestvalue up to our parent. */
                assert(record->parent != NULL);
                /* Namely, this record is a child of its parent, and this
                 * record has not yet reported. */
                assert(record->parent->unreported_children > 0);
                assert(record->hasbestvalue);

                Value value_to_parent;
                if (record->attacker != record->parent->attacker) {
                    value_to_parent = -record->bestvalue;
                } else {
                    value_to_parent = record->bestvalue;
                }
                if (!record->parent->hasbestvalue || value_to_parent > record->parent->bestvalue) {
                    record->parent->hasbestvalue = true;
                    record->parent->bestvalue = value_to_parent;
                    record->parent->bestmove = record->move;
                }
                record->parent->unreported_children -= 1;
                delete record;
                continue;
            }
        }

        assert(record->type == RECURSE);
        assert(record->parent != NULL);
        assert(record->parent->type == RETURN);
        /* Namely, this record is a child of its parent, and this record has
         * not yet reported. */
        assert(record->parent->unreported_children > 0);

        State newstate = record->st;
        this->applymove(newstate, record->move);
        const int newattacker = this->findattacker(newstate);

        /* Now this is basically the same code as depth_first(). */

        /* If we're wrapping up already, then we'll just take the value
         * of this position to be its value according to this->evaluate().
         * We'll do the same easy evaluate() route if the attacker has
         * no possible moves.
         */
        if (insertednodes == maxnodes) {
      easy_evaluate:
            Value value_to_me = this->evaluate2(record->st, newstate);
            Value value_to_parent;
            if (this->findattacker(record->st) != record->parent->attacker) {
                value_to_parent = -value_to_me;
            } else {
                value_to_parent = value_to_me;
            }

            if (!record->parent->hasbestvalue || value_to_parent > record->parent->bestvalue) {
                record->parent->hasbestvalue = true;
                record->parent->bestvalue = value_to_parent;
                record->parent->bestmove = record->move;
            }
            record->parent->unreported_children -= 1;
            delete record;
            continue;
        }

        assert(insertednodes < maxnodes);
        /* Otherwise, if we are not yet in the wrapping-up stage, the value
         * of this position is going to be computed by taking the maximum of
         * the values of all its children. Push a record for each child, where
         * that record contains a link to the new RETURN record. */
        std::vector<Move> allmoves;
        this->findmoves(newstate, allmoves);
        if (allmoves.empty()) {
            /* If the new attacker has no moves left, then the game is
             * definitely over. We must evaluate this position to see how
             * happy our parent is to defend it. See above. */
            goto easy_evaluate;
        }

        /* The new attacker has some possible moves, and we're not yet
         * wrapping things up. So push a RECURSE record for each possible
         * move, and push a RETURN record to defer the final processing on
         * this node.
         */
        BFRecord *return_record = new BFRecord(RETURN, record->move, (int)allmoves.size(), newattacker, record->parent);
        for (int i=0; i < (int)allmoves.size(); ++i) {
            Q.push(new BFRecord(RECURSE, newstate, allmoves[i], return_record));
            insertednodes += 1;
            if (insertednodes == maxnodes) {
                return_record->unreported_children = i+1;
                break;
            }
        }
        Q.push(return_record);

        /* We have now successfully suspended the rest of this computation.
         * This record can now be disposed of. */
        delete record;
        assert(insertednodes <= maxnodes);
        /* If insertednodes == maxnodes at this point, we enter the
         * "wrapping things up" phase (see above), in which we start using
         * this->evaluate() on whatever nodes of the game tree still remain
         * in Q, rather than calling findmoves() on them.
         */
    }
    /* The loop above will exit only if the queue becomes empty. This should
     * never happen, because before the queue can empty completely we must
     * encounter "top_level_return_record", and at that point we'll jump to
     * "delete_any_remaining_queue_elements" below. */
    assert(false);

  delete_any_remaining_queue_elements:
    delete Q.front();
    Q.pop();
    assert(Q.empty());
    while (!Q.empty()) {
        delete Q.front();
        Q.pop();
    }
    return true;
}
