
#include <algorithm>
#include <assert.h>
#include "state.h"
#include "move.h"
#include "InferMove.h"
#include "ApplyMove.h"


static int count_unconstrained_catastrophes(const WholeMove &move, int ax)
{
    /* If "move" contains catastrophe actions of the form "cat" --- as 
     * opposed to "cat red" or "cat at Alpha" or "cat red at Alpha" ---
     * then we can make those catastrophes in arbitrary order and the
     * resulting state will be the same, as long as the number of
     * overpopulations in the current state is equal to the number of
     * catastrophes specified by the user. */
    int count = 0;
    assert(ax < (int)move.actions.size());
    assert(move.actions[ax].kind == CATASTROPHE);
    for (int i = ax; i < (int)move.actions.size(); ++i) {
        const SingleAction &action = move.actions[i];
        if (action.kind != CATASTROPHE)
          break;
        if (action.where != "" || action.color != UNKNOWN_COLOR)
          return 0;
        count += 1;
    }
    return count;
}


static bool infer_catastrophe(const GameState &st, const WholeMove &move, SingleAction &action)
{
    assert(action.kind == CATASTROPHE);
    /* Figure out how many catastrophes are being made after this one.
     * This is so that we can handle "sac y2; ...; cat; cat".
     * However, we're not going to handle arbitrary constraints;
     * just look for "cat" with color and star unknown. */
    const int numcats = count_unconstrained_catastrophes(move, &action - &move.actions[0]);
    const bool order_matters = (numcats == 0);
    SingleAction newaction = action;
    int found = 0;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        if (action.where != "" && here.name != action.where)
          continue;
        for (Color c = RED; c <= BLUE; ++c) {
            if (action.color != UNKNOWN_COLOR && c != action.color)
              continue;
            if (here.numberOf(c) >= 4) {
                if (found != 0) {
                    if (order_matters || found == numcats) {
                        /* We can't deal with this ambiguity. */
                        return false;
                    }
                } else {
                    newaction.where = here.name;
                    newaction.color = c;
                }
                found += 1;
            }
        }
    }
    action = newaction;
    if (order_matters) {
        assert(found == 0 || found == 1);
        return (found != 0);
    } else {
        assert(found <= numcats);
        return (found == numcats);
    }
}


static bool infer_sacrifice(const GameState &st, int attacker, const WholeMove &move, SingleAction &action)
{
    assert(action.kind == SACRIFICE);
    int ax;
    for (ax=0; move.actions[ax].kind != SACRIFICE; ++ax)
      continue;
    int num_actions = 0;
    Color needed_color = UNKNOWN_COLOR;
    for (++ax; ax < (int)move.actions.size(); ++ax) {
        if (move.actions[ax].kind == CATASTROPHE)
          break;
        switch (move.actions[ax].kind) {
            case SACRIFICE: assert(false); break;
            case CAPTURE:
                if (needed_color != UNKNOWN_COLOR && needed_color != RED) return false;
                needed_color = RED;
                break;
            case MOVE:
            case MOVE_CREATE:
                if (needed_color != UNKNOWN_COLOR && needed_color != YELLOW) return false;
                needed_color = YELLOW;
                break;
            case BUILD:
                if (needed_color != UNKNOWN_COLOR && needed_color != GREEN) return false;
                needed_color = GREEN;
                break;
            case CONVERT:
                if (needed_color != UNKNOWN_COLOR && needed_color != BLUE) return false;
                needed_color = BLUE;
                break;
            default: assert(false);
        }
        num_actions += 1;
    }
    assert(0 <= num_actions && num_actions <= 3);
    Size min_needed_size = (num_actions == 3 ? LARGE : (num_actions == 2 ? MEDIUM : SMALL));
    if (action.size != UNKNOWN_SIZE && action.size < min_needed_size)
      return false;
    if (needed_color != UNKNOWN_COLOR) {
        if (action.color == UNKNOWN_COLOR)
          action.color = needed_color;
        else if (action.color != needed_color)
          return false;
    }

    SingleAction newaction = action;
    bool foundone = false;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        if (action.where != "" && here.name != action.where)
          continue;
        if (here.ships[attacker].empty()) continue;
        for (Color c = RED; c <= BLUE; ++c) {
            if (action.color != UNKNOWN_COLOR && c != action.color)
              continue;
            /* Can't sacrifice the last ship at your own homeworld. */
            if (c != YELLOW && here.homeworldOf == attacker && here.ships[attacker].number() == 1)
              continue;
            for (Size s = min_needed_size; s <= LARGE; ++s) {
                if (action.size != UNKNOWN_SIZE && s != action.size)
                  continue;
                if (here.ships[attacker].numberOf(c,s) != 0) {
                    if (foundone) return false;
                    newaction.where = here.name;
                    newaction.color = c;
                    newaction.size = s;
                    foundone = true;
                }
            }
        }
    }
    action = newaction;
    return foundone;
}


static bool infer_multicapture(const GameState &st, int attacker, WholeMove &move, SingleAction &action)
{
    assert(action.kind == CAPTURE);
    const int defender = 1-attacker;
    /* Count the ships we're capturing with the equivalent of "capture r1y1g1". */
    PieceCollection to_capture;
    for (int j = (&action - &move.actions[0]); j < (int)move.actions.size(); ++j) {
        SingleAction &actjon = move.actions[j];
        if (actjon.kind != CAPTURE || actjon.where != "")
          break;
        if (actjon.color == UNKNOWN_COLOR || actjon.size == UNKNOWN_SIZE)
          break;
        to_capture.insert(actjon.color, actjon.size);
    }
    /* Now try to capture any ships matching that set. */
    PieceCollection captured;
    int j = (&action - &move.actions[0]);
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        /* Since we only try infer_multicapture() when we've already seen a sacrifice,
         * that means that we don't need to check for hasAccessTo(RED) here. */
        if (here.ships[attacker].empty()) continue;
        if (here.ships[defender].empty()) continue;
        const Size biggest = here.ships[attacker].biggestSize();
        for (Color c = RED; c <= BLUE; ++c) {
            for (Size s = SMALL; s <= biggest; ++s) {
                const int target = to_capture.numberOf(c,s);
                int num_here = here.ships[defender].numberOf(c,s);
                if (target != 0 && num_here != 0) {
                    /* One of our targets is here! */
                    if (captured.numberOf(c,s) != 0 && (captured.numberOf(c,s) + num_here > target))
                      return false;  /* Ambiguity! */
                    num_here = std::min(num_here, target - captured.numberOf(c,s));
                    captured.insert(c,s,num_here);
                    for ( ; num_here != 0; --num_here) {
                        assert(move.actions[j].kind == CAPTURE);
                        assert(move.actions[j].where == "");
                        move.actions[j].where = here.name;
                        move.actions[j].color = c;
                        move.actions[j].size = s;
                        ++j;
                    }
                }
            }
        }
    }
    assert(captured.contains(to_capture));
    if (captured != to_capture)
      return false;
    assert(j == (&action - &move.actions[0]) + to_capture.number());
    return true;
}


static bool infer_capture(const GameState &st, int attacker, WholeMove &move, SingleAction &action, bool saw_sacrifice)
{
    assert(action.kind == CAPTURE);

    if (saw_sacrifice && action.where == "" &&
            action.color != UNKNOWN_COLOR && action.size != UNKNOWN_SIZE) {
        /* This can handle "capture y2; capture y2" where the two y2 ships
         * are in two different systems. */
        return infer_multicapture(st, attacker, move, action);
    }

    const int defender = 1-attacker;
    SingleAction newaction = action;
    bool foundone = false;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        if (action.where != "" && here.name != action.where)
          continue;
        if (!saw_sacrifice && !here.playerHasAccessTo(attacker, RED))
          continue;
        if (here.ships[attacker].empty()) continue;
        if (here.ships[defender].empty()) continue;
        const Size biggest = here.ships[attacker].biggestSize();
        for (Color c = RED; c <= BLUE; ++c) {
            if (action.color != UNKNOWN_COLOR && c != action.color)
              continue;
            for (Size s = SMALL; s <= biggest; ++s) {
                if (action.size != UNKNOWN_SIZE && s != action.size)
                  continue;
                if (here.ships[defender].numberOf(c,s) == 0)
                  continue;
                if (foundone) return false;
                newaction.where = here.name;
                newaction.color = c;
                newaction.size = s;
                foundone = true;
            }
        }
    }
    action = newaction;
    return foundone;
}


static bool infer_movement_from(const GameState &st, int attacker, SingleAction &action, bool saw_sacrifice)
{
    assert(action.kind == MOVE || action.kind == MOVE_CREATE);
    assert(action.color != UNKNOWN_COLOR);
    assert(action.size != UNKNOWN_SIZE);
    assert(action.where == "");
    assert(action.whither != "");
    bool foundone = false;
    const StarSystem *whither = st.systemNamed(action.whither.c_str());
    if ((whither == NULL) != (action.kind == MOVE_CREATE))
      return false;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        if (here.homeworldOf == attacker && here.ships[attacker].number() == 1)
          continue;
        if (!saw_sacrifice && !here.playerHasAccessTo(attacker, YELLOW))
          continue;
        /* The inferred "where" must be adjacent to the given "whither". */
        if (action.kind == MOVE_CREATE && here.star.numberOf(action.newsize) != 0)
          continue;
        if (action.kind == MOVE && !here.isAdjacentTo(*whither))
          continue;
        /* A ship of the appropriate type must exist here. */
        if (here.ships[attacker].numberOf(action.color, action.size) != 0) {
            if (foundone) return false;
            action.where = here.name;
            foundone = true;
        }
    }
    return foundone;
}


static bool infer_build(const GameState &st, int attacker, SingleAction &action, bool saw_sacrifice)
{
    assert(action.kind == BUILD);
    SingleAction newaction = action;
    bool foundone = false;
    for (Color c = RED; c <= BLUE; ++c) {
        if (action.color != UNKNOWN_COLOR && c != action.color)
          continue;
        if (st.stash.numberOf(c) == 0)
          continue;
        const Size s = st.stash.smallestSizeOf(c);
        if (action.size != UNKNOWN_SIZE && s != action.size)
          continue;
        /* Can we build this color anywhere? */
        for (int i=0; i < (int)st.stars.size(); ++i) {
            const StarSystem &here = st.stars[i];
            if (action.where != "" && here.name != action.where) continue;
            if (!saw_sacrifice && !here.playerHasAccessTo(attacker, GREEN))
              continue;
            if (here.ships[attacker].numberOf(c) != 0) {
                if (foundone) return false;
                newaction.where = here.name;
                newaction.color = c;
                newaction.size = s;
                foundone = true;
            }
        }
    }
    action = newaction;
    return foundone;
}


static bool infer_convert(const GameState &st, int attacker, SingleAction &action, bool saw_sacrifice)
{
    assert(action.kind == CONVERT);
    SingleAction newaction = action;
    bool foundone = false;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &here = st.stars[i];
        if (action.where != "" && here.name != action.where)
          continue;
        if (!saw_sacrifice && !here.playerHasAccessTo(attacker, BLUE))
          continue;
        if (here.ships[attacker].empty()) continue;
        for (Color c = RED; c <= BLUE; ++c) {
            if (action.color != UNKNOWN_COLOR && c != action.color)
              continue;
            if (action.newcolor != UNKNOWN_COLOR && c == action.newcolor)
              continue;
            for (Size s = SMALL; s <= LARGE; ++s) {
                if (action.size != UNKNOWN_SIZE && s != action.size)
                  continue;
                if (here.ships[attacker].numberOf(c,s) == 0)
                  continue;
                /* What color might we turn this ship? */
                for (Color nc = RED; nc <= BLUE; ++nc) {
                    if (action.newcolor != UNKNOWN_COLOR && nc != action.newcolor)
                      continue;
                    if (nc == c) continue;
                    if (st.stash.numberOf(nc,s) != 0) {
                        if (foundone) return false;
                        newaction.where = here.name;
                        newaction.color = c;
                        newaction.size = s;
                        newaction.newcolor = nc;
                        newaction.newsize = s;
                        foundone = true;
                    }
                }
            }
        }
    }
    action = newaction;
    return foundone;
}


/* Given a move with missing pieces (e.g., "build g1" instead of
 * "build g1 at Earth"), infer the missing pieces from the state of
 * the game (for example, if the only place g1 could be built is at
 * Earth, then "build g1" must mean "build g1 at Earth").
 *   If the intended move is unambiguous, fill it in and return true.
 * If the move matches multiple non-equivalent possibilities, return
 * false (trashing the input move in the process).
 */
bool inferMoveFromState(const GameState &st, int attacker, WholeMove &move)
{
    if (!move.is_missing_pieces())
      return true;
    GameState newst = st;
    bool saw_sacrifice = false;
    for (int i=0; i < (int)move.actions.size(); ++i) {
        SingleAction &action = move.actions[i];
        switch (action.kind) {
            case CATASTROPHE:
                if (action.where != "" && action.color != UNKNOWN_COLOR)
                  break;
                if (!infer_catastrophe(newst, move, action)) return false;
                break;
            case SACRIFICE:
                saw_sacrifice = true;
                if (action.where != "" && action.color != UNKNOWN_COLOR && action.size != UNKNOWN_SIZE)
                  break;
                if (!infer_sacrifice(newst, attacker, move, action)) return false;
                break;
            case CAPTURE:
                if (action.where != "" && action.color != UNKNOWN_COLOR && action.size != UNKNOWN_SIZE)
                  break;
                if (!infer_capture(newst, attacker, move, action, saw_sacrifice)) return false;
                break;
            case MOVE:
            case MOVE_CREATE:
                /* We currently can't infer that e.g. in the situation
                 * Home (0,g3b2) y1-r1
                 * Alpha (b1) g2-
                 * Beta (b2) r2-
                 * "sacrifice; move" must mean "sacrifice y1 at Home; move g2 from Alpha to Home"
                 * because g2 is the only ship able to reach Home by the end of the turn to prevent
                 * a loss.
                 */
                if (action.whither == "") return false;
                if (action.kind == MOVE_CREATE) {
                    if (action.newcolor == UNKNOWN_COLOR || action.newsize == UNKNOWN_SIZE)
                      return false;
                }
                if (action.color == UNKNOWN_COLOR || action.size == UNKNOWN_SIZE)
                  return false;
                if (action.where != "")
                  break;
                if (!infer_movement_from(newst, attacker, action, saw_sacrifice)) return false;
                break;
            case BUILD:
                if (action.where != "" && action.color != UNKNOWN_COLOR && action.size != UNKNOWN_SIZE)
                  break;
                if (!infer_build(newst, attacker, action, saw_sacrifice)) return false;
                break;
            case CONVERT:
                assert(action.newsize == action.size);
                if (action.where != "" && action.color != UNKNOWN_COLOR && action.size != UNKNOWN_SIZE
                                       && action.newcolor != UNKNOWN_COLOR)
                  break;
                if (!infer_convert(newst, attacker, action, saw_sacrifice)) return false;
                break;
        }
        /* Now we have either filled in the missing pieces of "action",
         * or bailed out by returning false. */
        assert(!action.is_missing_pieces());
        if (!ApplyMove::Single(newst, attacker, action))
          return false;
    }
    return true;
}
