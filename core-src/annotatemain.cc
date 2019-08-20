
/* Provide a prompt; let the user enter moves for Player 1 and Player 2
 * on alternating turns, from a game taking place in the real world.
 * When the last move opened up a winning condition for either player,
 * print a warning --- either the current player is "in check" and must
 * respond, or else the player who just moved made a fatal mistake.
 * Provide the command "undo" to take back an arbitrary number of moves.
 */

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "getline.h"
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include "state.h"
#include "AI.h"
#include "AllMoves.h"
#include "ApplyMove.h"
#include "InferMove.h"
#include "PlanetNames.h"
#include "WholeMove.h"

#define randint0(n) (rand() % (n))
#define do_crash do_error

static std::string g_playerNames[2];
static bool g_Verbose;
static bool g_ReportBlunders;
static bool g_VerifyTranscript;
static bool g_SDGFormat;

static std::string convertToString(const GameState& st) {
    return st.toString();
}

static std::string convertToString(const PieceCollection& pc) {
    return pc.toString();
}

static std::string convertToString(const WholeMove& move) {
    if (g_SDGFormat) return move.toSDGString();
    return move.toString();
}

class History {
    struct Node {
        WholeMove move;
        GameState st;     /* the state *after* this move */
    };
    /* hvec[0] is the initial state; its "move" is garbage; it can't be undone. */
    std::vector<Node> hvec;
    /* hvec[hidx] is the last (non-undone) move. */
    int hidx;
  public:
    History(): hidx(0) {
        hvec.push_back(Node());
    }
    bool game_has_just_ended() const {
        return (hidx > 0) && hvec[hidx].st.gameIsOver() && !hvec[hidx-1].st.gameIsOver();
    }
    void setup(const GameState &st) {
        assert(hidx == 0);
        assert(hvec.size() == 1);
        hvec[0].st = st;
    }
    void showState(FILE *fp) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose) {
            fprintf(fp, "The current state is:\n");
        }
        fputs(convertToString(hvec[hidx].st).c_str(), fp);
    }
    void showStash(FILE *fp) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose) {
            fprintf(fp, "The current stash is:\n");
        }
        fprintf(fp, "%s\n", convertToString(hvec[hidx].st.stash).c_str());
    }
    void review(FILE *fp, bool verbose) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (verbose && hidx > 0) {
            fprintf(fp, "This game has gone on for %d moves.\n", hidx);
        }
        fprintf(fp, "%s", convertToString(hvec[0].st).c_str());
        for (int i=1; i <= hidx; ++i) {
            if (verbose) {
                fprintf(fp, "%d. %s: ", i, g_playerNames[1-(i%2)].c_str());
            }
            fprintf(fp, "%s\n", convertToString(hvec[i].move).c_str());
        }
        if (verbose && hidx > 0) {
            showState(fp);
        }
    }
    bool can_undo() const {
        return (hidx != 0);
    }
    bool undo() {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (hidx == 0) {
            puts("There are no moves to undo.");
            return false;
        } else {
            --hidx;
            if (g_Verbose) {
                puts("The last move has been undone.");
                puts("The current state is:");
                puts(convertToString(hvec[hidx].st).c_str());
            }
            return true;
        }
    }
    bool can_redo() const {
        return (hidx+1 != (int)hvec.size());
    }
    bool redo() {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (hidx+1 == (int)hvec.size()) {
            puts("There are no moves to redo.");
            return false;
        } else {
            ++hidx;
            if (g_Verbose) {
                printf("%d. %s: %s\n", hidx, g_playerNames[1-(hidx%2)].c_str(),
                    convertToString(hvec[hidx].move).c_str());
                puts("The move has been redone. The current state is:");
                puts(convertToString(hvec[hidx].st).c_str());
            }
            return true;
        }
    }
    void makemove(const WholeMove &move, int attacker) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        assert(attacker == (hidx % 2));
        hvec.resize(hidx+1);
        hvec.push_back(hvec[hidx]);
        ++hidx;
        auto UNUSED(result) = ApplyMove::Whole(hvec[hidx].st, attacker, move);
        assert(result == ApplyMove::Result::SUCCESS);
        hvec[hidx].move = move;
    }
    const GameState &currentState() const {
        return hvec[hidx].st;
    }
    const GameState &previousState() const {
        assert(hidx != 0);
        return hvec[hidx - 1].st;
    }
};

History g_History;


static void do_error(const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    printf("Error: ");
    vprintf(msg, ap);
    putchar('\n');
    va_end(ap);
    exit(EXIT_FAILURE);
}

static void do_help()
{
    puts("A \"move\" is a sequence of \"actions\", strung together with semicolons.");
    puts("For example: \"catastrophe red at Alpha; move b3 from Beta to Alpha\".");
    puts("Recognized actions are:");
    puts("  catastrophe <color> at <name>");
    puts("  sacrifice <piece> at <name>");
    puts("  pass");
    puts("  capture <piece> at <name>");
    puts("  move <piece> from <name> to <name>");
    puts("  move <piece> from <name> to <name> (<piece>)");
    puts("  build <piece> at <name>");
    puts("  convert <piece> to <piece> at <name>");
    puts("For example, to create a new star system named \"Gamma\", you might enter");
    puts("\"move r1 from Alpha to Gamma (b2)\". This would create a new system named");
    puts("\"Gamma\", with a medium blue star.");
    puts("The input parser is case-sensitive and whitespace-sensitive.\n");
    puts("Recognized meta-commands are:");
    puts("  help");
    puts("  brief, verbose");
    puts("  undo, redo");
    puts("  state");
    puts("  stash");
    puts("  review, review <filename>");
    puts("  ai_move");
    puts("  count_moves, rate_moves");
    puts("  quit");
    puts("The \"review\" command prints a transcript of the game up to this point,");
    puts("or writes the same transcript to the specified text file.");
    puts("The \"state\" command prints the current state of the game.");
    puts("The \"ai_move\" command instructs the computer to pick a reasonable move for");
    puts("the current player, and then have the current player make that move.");
    return;
}


static void setup_ai(GameState &st, StarSystem &hw)
{
    assert(&hw == &st.stars.back());
    const int attacker = hw.homeworldOf;
    assert(attacker == 0 || attacker == 1);
    const StarSystem * const opponent_hw = st.homeworldOf(1-attacker);

    while (1) {
        hw.star.clear();
        hw.ships[attacker].clear();
        /* The star should contain two colors, including at least one yellow or blue. */
        const Color c1 = (randint0(2)==0 ? YELLOW : BLUE);
        const Color c2 = Color((1+randint0(3)+(int)c1) % 4);
        /* The star should contain two different sizes. */
        const Size s1 = Size(randint0(3));
        const Size s2 = Size((s1+1+randint0(2)) % 3);
        hw.star.insert(c1, s1);
        hw.star.insert(c2, s2);
        if (opponent_hw != nullptr) {
            /* Make sure the homeworlds are as far apart as possible.
             * This condition disallows (r1y1, g2b3) and (r1y2, g1b2)
             * while still allowing (r1y1, g1b2) and (r1y2, g1b3). */
            if (opponent_hw->star.numberOf(s1) == opponent_hw->star.numberOf(s2)) {
                continue;
            }
        }
        /* The initial ship should be big and non-red. If the star doesn't have green
         * already, then the initial ship must be green. Otherwise, the ship must be
         * of a color that's not already in the star itself. */
        const Color shipc = (c2 != GREEN) ? GREEN : (c1 == YELLOW) ? BLUE : YELLOW;
        hw.ships[attacker].insert(shipc, LARGE);
        /* If this configuration isn't actually possible, rinse and repeat. */
        if (!st.stash.contains(hw.pieceCollection())) {
            continue;
        }
        /* This configuration is okay. */
        break;
    }
    st.stash -= hw.star;
    st.stash -= hw.ships[attacker];
    return;
}


static void setup_human(GameState &st, int attacker)
{
    printf("%s, set up your homeworld.\n", g_playerNames[attacker].c_str());

    assert(StarSystem::isValidName(g_playerNames[attacker].c_str()));
    st.stars.push_back(StarSystem(g_playerNames[attacker].c_str()));
    StarSystem &hw = st.stars.back();
    hw.homeworldOf = attacker;

    char *moveline = nullptr;
    char *result;
    printf("Enter the initial pieces for your star. For example, \"y3b2\". > "); fflush(stdout);
  get_star_pieces:
    result = getline_113(&moveline);
    /* Ignore input errors for the sake of simplicity. TODO FIXME BUG HACK */
    assert(result == moveline && result != nullptr);
    if (strcmp(moveline, "ai_move") == 0) {
        free(moveline);
        printf("AI is setting up a homeworld for %s...\n", g_playerNames[attacker].c_str());
        setup_ai(st, hw);
        return;
    }
    PieceCollection pc;
    bool success = pc.scan(moveline);
    if (!success) {
        puts("Your input did not appear to be a valid piece collection.");
        puts("A piece collection consists of alternating colors (rygb) and sizes (123),");
        puts("case-sensitive, with no intervening spaces or punctuation. For example,");
        puts("\"y3b2\" or \"r1r2\" or \"g1b1\". Enter \"ai_move\" for an AI setup.");
        printf("You entered: \"%s\"\n", moveline);
        printf("Please enter a valid piece collection for your star. > "); fflush(stdout);
        free(moveline);
        goto get_star_pieces;
    }
    /* The PieceCollection was syntactically valid, but in order to be semantically valid
     * it must consist of exactly two pieces, and those pieces must be available in the stash. */
    if (pc.number() != 2) {
        printf("Your star must contain two pieces, not %d.\n", pc.number());
        printf("Please re-enter the initial pieces for your star. > "); fflush(stdout);
        free(moveline);
        goto get_star_pieces;
    }
    if (!st.stash.contains(pc)) {
        /* Note that the only way for the stash not to contain a collection of
         * only two pieces is for the AI player to have built first --- and to
         * have picked two pieces of the same color and size, which is highly
         * unlikely to be a good starting move!  A pristine stash contains
         * three pieces of each denomination. */
        puts("Your star must consist of pieces available in the stash.");
        puts("(Remember, your opponent built first.) The stash contains:");
        printf("%s\n", convertToString(st.stash).c_str());
        printf("Please re-enter the initial pieces for your star. > "); fflush(stdout);
        free(moveline);
        goto get_star_pieces;
    }
    free(moveline);
    hw.star = pc;
    st.stash -= pc;

    printf("Enter your starting ship. For example, \"g3\". > "); fflush(stdout);
  get_ship:
    result = getline_113(&moveline);
    /* Ignore input errors for the sake of simplicity. TODO FIXME BUG HACK */
    assert(result == moveline && result != nullptr);
    success = pc.scan(moveline);
    if (!success) {
        printf("Your input did not appear to be a valid piece collection.\n");
        printf("A piece collection consists of alternating colors (rygb) and sizes (123),\n");
        printf("case-sensitive, with no intervening spaces or punctuation. For example,\n");
        printf("\"y3\" or \"r2\" or \"g1\". You entered: \"%s\"\n", moveline);
        printf("Please enter a valid piece collection for your ship. > "); fflush(stdout);
        free(moveline);
        goto get_ship;
    }
    /* The PieceCollection was syntactically valid, but in order to be semantically valid
     * it must consist of exactly one piece, and that piece must be available in the stash. */
    if (pc.number() != 1) {
        printf("You must pick one starting ship, not %d.\n", pc.number());
        printf("Please re-enter your starting ship. > "); fflush(stdout);
        free(moveline);
        goto get_ship;
    }
    if (!st.stash.contains(pc)) {
        /* Note that the only way for the stash not to contain a collection of
         * only three pieces is for the other player to have built first --- and
         * to have picked two pieces of the same color and size, which is highly
         * unlikely to be a good starting move!  A pristine stash contains
         * three pieces of each denomination. */
        printf("You must pick a ship from the pieces available in the stash.\n");
        printf("(Remember, your opponent built first.) The stash contains:\n");
        printf("%s\n", convertToString(st.stash).c_str());
        printf("Please re-enter your starting ship. > "); fflush(stdout);
        free(moveline);
        goto get_ship;
    }
    free(moveline);
    hw.ships[attacker] = pc;
    st.stash -= pc;
    return;
}


/* Returns true if "attacker" had a non-losing move, but
 * chose to move into check instead. */
static bool was_boneheaded_move(const GameState& oldst, int attacker, const WholeMove&, const GameState& newst)
{
    if (newst.gameIsOver()) {
        return false;
    }
    if (!findWinningMove(newst, 1-attacker, nullptr)) {
        return false;
    }
    /* The attacker did move into check.
     * But maybe he didn't have any choice? */
    return findAllMoves(
        oldst,
        attacker,
        /*prune_obviously_worse_moves=*/true,
        /*look_only_for_wins=*/false,
        0xF,
        [&](const WholeMove&, const GameState& newst) {
            /* If he'd made this move instead, would he still be in check? */
            if (newst.gameIsOver()) {
                /* No, in fact he'd have won the game! */
                return true;
            }
            if (!findWinningMove(newst, 1-attacker, nullptr)) {
                /* No, he wouldn't still be in check. */
                return true;
            }
            return false;
        }
    );
}

#if 0
static bool was_bluebird_move(const GameState& oldst, int attacker, const WholeMove& m, const GameState&)
{
    const StarSystem *defender_hw = oldst.homeworldOf(1-attacker);
    assert(defender_hw != nullptr);
    if (!(
          m.actions.size() == 5 &&
          m.actions[0].kind == SACRIFICE &&
          (m.actions[1].kind == MOVE || m.actions[1].kind == MOVE_CREATE) &&
          (m.actions[2].kind == MOVE || m.actions[2].kind == MOVE_CREATE) &&
          (m.actions[3].kind == MOVE || m.actions[3].kind == MOVE_CREATE) &&
          m.actions[4].kind == CATASTROPHE
    )) return false;
    Color catcolor = m.actions[4].color;
    if (m.actions[4].where != defender_hw->name) return false;
    if (defender_hw->pieceCollection().numberOf(catcolor) != 2) return false;
    if (m.actions[1].color != catcolor || m.actions[2].color != catcolor || m.actions[3].color != catcolor) return false;
    if (!(
        (m.actions[1].whither == defender_hw->name && m.actions[2].whither == m.actions[3].where && m.actions[3].whither == defender_hw->name) ||
        (m.actions[2].whither == defender_hw->name && m.actions[1].whither == m.actions[3].where && m.actions[3].whither == defender_hw->name) ||
        (m.actions[3].whither == defender_hw->name && m.actions[1].whither == m.actions[2].where && m.actions[2].whither == defender_hw->name)
    )) return false;
    return true;
}

static bool was_yellowbird_move(const GameState& oldst, int attacker, const WholeMove& m, const GameState& newst)
{
    const StarSystem *attacker_hw = oldst.homeworldOf(attacker);
    assert(attacker_hw != nullptr);
    if (!(
          m.actions.size() == 4 &&
          m.actions[0].kind == SACRIFICE && m.actions[0].where == attacker_hw->name &&
          m.actions[1].kind == BUILD &&
          m.actions[2].kind == BUILD &&
          m.actions[3].kind == BUILD
    )) return false;
    if (attacker_hw->ships[attacker].numberOf(GREEN) != 1) return false;
    if (attacker_hw->ships[attacker].numberOf(LARGE) != 1) return false;
    if (oldst.stash.numberOf(RED) && oldst.stash.smallestSizeOf(RED) == LARGE) return false;
    if (oldst.stash.numberOf(YELLOW) && oldst.stash.smallestSizeOf(YELLOW) == LARGE) return false;
    if (oldst.stash.numberOf(GREEN) && oldst.stash.smallestSizeOf(GREEN) == LARGE) return false;
    if (oldst.stash.numberOf(BLUE) && oldst.stash.smallestSizeOf(BLUE) == LARGE) return false;

    const StarSystem *newhw = newst.homeworldOf(attacker);
    assert(newhw != nullptr);
    if (newhw->ships[attacker].numberOf(LARGE) == 0) return false;

    bool can_build_large = findAllMoves(
        newst,
        1-attacker,
        /*prune_obviously_worse_moves=*/false,
        /*look_only_for_wins=*/false,
        (1u << GREEN),
        [&](const WholeMove& move, const GameState&) {
            for (const auto& action : move.actions) {
                if (action.kind == BUILD && action.size == LARGE) {
                    return true;
                }
            }
            return false;
        }
    );
    if (can_build_large) return false;
    return true;
}
#endif

static void make_move_and_report(int attacker, const WholeMove& move)
{
    g_History.makemove(move, attacker);
    const GameState& oldst = g_History.previousState();
    const GameState& newst = g_History.currentState();
    bool game_is_over = newst.gameIsOver();

    if (move.isHomeworld()) {
        const StarSystem *hw = newst.homeworldOf(attacker);
        assert(hw != nullptr);
        g_playerNames[attacker] = hw->name;
    }

    if (g_ReportBlunders) {
        if (was_boneheaded_move(oldst, attacker, move, newst)) {
            printf("%s blundered into check on this move:\n%s\n",
                g_playerNames[attacker].c_str(), convertToString(move).c_str());
            printf("The position was:\n");
            printf("%s\n", convertToString(newst).c_str());
        }
#if 0
        if (was_bluebird_move(oldst, attacker, move, newst)) {
            if (game_is_over) {
                printf("BLUEBIRD WIN!  ");
                int aships = 0;
                int dships = 0;
                for (const auto& ss : oldst.stars) {
                    aships += ss.ships[attacker].number();
                    dships += ss.ships[1-attacker].number();
                }
                printf("winner had %d %c %d ships\n", aships, "<=>"[(aships < dships ? -1 : aships > dships) + 1], dships);
            } else {
                printf("BLUEBIRD MOVE!\n");
            }
        }
        if (was_yellowbird_move(oldst, attacker, move, newst)) {
            printf("YELLOWBIRD MOVE!  ");
            const StarSystem *hw = newst.homeworldOf(attacker);
            assert(hw != nullptr);
            if (hw->ships[attacker].numberOf(RED, LARGE) >= 1) printf(" red");
            if (hw->ships[attacker].numberOf(YELLOW, LARGE) >= 1) printf(" yellow");
            if (hw->ships[attacker].numberOf(GREEN, LARGE) >= 1) printf(" green");
            if (hw->ships[attacker].numberOf(BLUE, LARGE) >= 1) printf(" blue");
            printf("\n");
        }
#endif
    }

    if (game_is_over) {
        if (g_Verbose) {
            printf("%s has won the game!\n", g_playerNames[attacker].c_str());
            puts("(Valid commands at this point include \"review\" and \"help\".)");
        }
    }

    if (g_Verbose && !game_is_over && newst.containsOverpopulation()) {
        static bool warning_given = false;
        if (!warning_given) {
            printf("%s has left overpopulations on the board.\n", g_playerNames[attacker].c_str());
            printf("The \"ai_move\" command may not work properly in this situation.\n");
            printf("Consider using \"undo\" and adding one or more \"catastrophe\" actions\n");
            printf("to the end of the last move.\n");
            warning_given = true;
        } else {
            printf("(%s has left overpopulations on the board.)\n", g_playerNames[attacker].c_str());
        }
    }
}


static bool move_and_record(int attacker)
{
    const bool game_is_over = g_History.game_has_just_ended();
    char *moveline_cstr = nullptr;
    std::string moveline;

    while (true) {
        /* Prompt for the human's move, and read in a line. */
        if (g_Verbose) {
            printf("%s's move? > ", g_playerNames[attacker].c_str()); fflush(stdout);
        }
        char *result = getline_113(&moveline_cstr);
        assert(result == moveline_cstr || result == nullptr);
        /* Act as if there's an implicit "quit" at the end of the input;
         * otherwise, we'd just go on reading the last command forever. */
        if (result == nullptr) {
            moveline = "quit";
        } else {
            moveline = moveline_cstr;
            free(moveline_cstr);
        }

        if (moveline == "") {
            if (g_Verbose) {
                puts("Enter the string \"help\" for help with this game's interface.");
            }
        } else if (moveline == "help") {
            do_help();
        } else if (moveline == "quit") {
            return false;
        } else if (moveline == "verbose") {
            puts("Now operating in verbose mode.");
            g_Verbose = true;
        } else if (moveline == "brief") {
            if (g_Verbose) {
                puts("Now operating in non-verbose \"brief\" mode.");
                puts("No transcript will be saved if you \"quit\" while in brief mode!");
            }
            g_Verbose = false;
        } else if (moveline[0] == '#') {
            /* This is a comment in a transcript file; ignore it. */
        } else if (moveline == "undo") {
            const bool success = g_History.undo();
            if (success) return true;
        } else if (moveline == "redo") {
            const bool success = g_History.redo();
            if (success) return true;
        } else if (moveline == "review") {
            g_History.review(stdout, g_Verbose);
        } else if (moveline == "state") {
            g_History.showState(stdout);
        } else if (moveline == "stash") {
            g_History.showStash(stdout);
        } else if (strncmp(moveline.c_str(), "review ", 7) == 0) {
            const char *filename = &moveline[7];
            FILE *out = fopen(filename, "w");
            if (out == nullptr) {
                printf("File \"%s\" could not be opened for writing.\n", filename);
            } else {
                g_History.review(out, /*verbose=*/false);
                fclose(out);
                if (g_Verbose) {
                    printf("A transcript up to this point has been saved to file \"%s\".\n", filename);
                }
            }
        } else if (game_is_over) {
            /* Once the game is over you're not allowed to continue making
             * moves, but you can still use the bookkeeping commands above
             * this check. */
            if (g_Verbose) {
                puts("The game is over; only \"undo\" and \"review\" are allowed\n"
                       "at this point. You entered:");
                printf("\"%s\"\n", moveline.c_str());
                puts("Enter the string \"help\" for help with this game's interface.\n");
            } else {
                puts("The game is already over!");
            }
            if (g_VerifyTranscript) {
                puts("The state prior to this move was:");
                g_History.showState(stdout);
                do_error("Transcript is incorrect.");
            }
        } else if (moveline == "rate_position") {
            int rating = ai_static_evaluation(g_History.currentState(), 1-attacker);
            printf("%d\n", rating);
        } else if (moveline == "rate_moves") {
            (void)get_all_moves_sorted_by_value(g_History.currentState(), attacker, true);
        } else if (moveline == "count_moves") {
            std::vector<WholeMove> allmoves = findAllMoves(
                g_History.currentState(), attacker,
                /*prune=*/false, /*wins=*/false, /*colors=*/0xf
            );
            assert(allmoves.size() >= 1);  /* "pass" from a legal position is always legal */
            printf("%d\n", (int)allmoves.size());
        } else {
            WholeMove move;
            const bool isAiMove = (moveline == "ai_move");
            if (isAiMove) {
                move = get_ai_move(g_History.currentState(), attacker);
                if (g_Verbose) {
                    printf("AI for %s chooses: %s\n", g_playerNames[attacker].c_str(), convertToString(move).c_str());
                }
                assert(ApplyMove::isValidMove(g_History.currentState(), attacker, move));
                make_move_and_report(attacker, move);
                return true;
            } else {
                const bool success = move.scan(moveline.c_str());
                if (!success) {
                    puts("The given string did not parse as a move. It was:");
                    printf("\"%s\"\n", moveline.c_str());
                    if (g_VerifyTranscript) {
                        puts("The state prior to this move was:");
                        g_History.showState(stdout);
                        do_error("Transcript is incorrect.");
                    }
                    puts("Enter the string \"help\" for help with this game's interface.");
                } else {
                    auto result = [&]() {
                        if (move.isMissingPieces()) {
                            WholeMove oldmove = move;
                            bool inferred = inferMoveFromState(g_History.currentState(), attacker, &move);
                            if (!inferred) {
                                move = oldmove;
                                return ApplyMove::Result::AMBIGUOUS;
                            }
                        }
                        GameState newst = g_History.currentState();
                        return ApplyMove::Whole(newst, attacker, move);
                    }();
                    if (result == ApplyMove::Result::SUCCESS) {
                        /* We got a completely valid move. */
                        if (g_Verbose) {
                            puts("Okay.");
                        }
                        make_move_and_report(attacker, move);
                        return true;
                    } else {
                        switch (result) {
                            case ApplyMove::Result::SUICIDE:
                                puts("The move as parsed was disallowed by the rule against self-destruction. The move was:");
                                break;
                            case ApplyMove::Result::UNKNOWN_NAME:
                                puts("The move as parsed referred to a nonexistent star system. The move was:");
                                break;
                            case ApplyMove::Result::DUPLICATE_NAME:
                                puts("The move as parsed tried to create a new star system with the same name as an existing one. The move was:");
                                break;
                            case ApplyMove::Result::AMBIGUOUS:
                                puts("The move as parsed was incomplete or ambiguous. The move was:");
                                break;
                            case ApplyMove::Result::IMPOSSIBLE:
                                puts("The move as parsed was disallowed by the rules. The move was:");
                                break;
                            case ApplyMove::Result::NOT_DURING_SETUP:
                                puts("The move as parsed was not permitted during the setup phase of the game. The move was:");
                                break;
                            case ApplyMove::Result::ONLY_DURING_SETUP:
                                puts("The move as parsed was not permitted outside of the setup phase of the game. The move was:");
                                break;
                            default:
                                assert(false);
                        }
                        printf("\"%s\"\n", move.toString().c_str());
                        if (g_VerifyTranscript) {
                            puts("The state prior to this move was:");
                            g_History.showState(stdout);
                            do_error("Transcript is incorrect.");
                        }
                        puts("Enter the string \"help\" for help with this game's interface.");
                    }
                }
            }
        }
    }
}

int main(int argc, char **argv)
{
    srand((unsigned int)time(nullptr));

    int arg_index;
    bool auto_setup = false;
    for (arg_index=1; arg_index < argc; ++arg_index) {
        if (argv[arg_index][0] != '-') break;
        std::string arg = argv[arg_index];
        if (arg == "--") { ++arg_index; break; }
        if (arg == "--blunders") {
            g_ReportBlunders = true;
        } else if (arg == "--sdg") {
            g_SDGFormat = true;
        } else if (arg == "--verify") {
            g_VerifyTranscript = true;
        } else if (arg == "--auto") {
            auto_setup = true;
        } else if (arg == "--seed") {
            if (arg_index+1 >= argc || !isdigit(argv[arg_index+1][0])) {
                do_error("The --seed argument requires an integer parameter!");
            }
            ++arg_index;
            srand((unsigned int)atoi(argv[arg_index]));
        } else {
            do_error("Unrecognized command-line argument %s", argv[arg_index]);
        }
    }

    if (g_ReportBlunders || g_VerifyTranscript) {
        auto_setup = true;
    }

    GameState initialState;

    if (auto_setup && arg_index == argc) {
        /* "annotate --auto" means that the input will be in the form of a
         * game transcript, and we should be quiet instead of verbose.
         * The transcript might begin with a game state, or it might begin
         * with "homeworld" moves. */
        g_Verbose = false;
        std::string firstline = initialState.scan(stdin);

        WholeMove firstmove;
        bool firstline_is_homeworld_move = (firstmove.scan(firstline.c_str()) && firstmove.isHomeworld());
        firstline += "\n";
        for (int i = firstline.length(); i > 0; --i) {
            int rc = ungetc(firstline[i-1], stdin);
            if (rc == EOF) {
                do_error("ungetc: %d character%s of input could not be pushed back.\n"
                    "Try adding a blank line after the homeworld setup lines.", i,
                    (i>1 ? "s" : ""));
            }
        }
        assignPlanetNames(&initialState);
        if (firstline_is_homeworld_move) {
            // g_playerNames will be initialized during the game.
            g_playerNames[0] = "Player 0";
            g_playerNames[1] = "Player 1";
        } else {
            StarSystem *hw = initialState.homeworldOf(0);
            if (hw == nullptr) {
                do_error("The initial homeworld setup didn't include Player 0's homeworld!");
            }
            g_playerNames[0] = hw->name;
            hw = initialState.homeworldOf(1);
            if (hw == nullptr) {
                do_error("The initial homeworld setup didn't include Player 1's homeworld!");
            }
            g_playerNames[1] = hw->name;
        }
    } else if (!auto_setup && arg_index+2 == argc) {
        /* "annotate Sam Dave" means that the input will be entered via the
         * keyboard as the game progresses, and we should be verbose
         * (acknowledging valid moves, prompting the user to re-enter invalid
         * moves, et cetera). */
        g_Verbose = true;
        if (!StarSystem::isValidName(argv[arg_index])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[arg_index]);
        } else if (!StarSystem::isValidName(argv[arg_index+1])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[arg_index+1]);
        }

        g_playerNames[0] = argv[arg_index];
        g_playerNames[1] = argv[arg_index+1];
        initialState.newGame();

        printf("%s will set up first and move first.\n", g_playerNames[0].c_str());
        setup_human(initialState, 0);

        printf("The state after %s's setup is:\n", g_playerNames[0].c_str());
        printf("%s\n", convertToString(initialState).c_str());
        setup_human(initialState, 1);

        printf("The state after both players' setup is:\n");
        printf("%s\n", convertToString(initialState).c_str());
    } else {
        do_error("Incorrect command-line arguments.\n"
                 "The recognized command lines are:\n"
                 "  annotate Sam Dave        verbosely set up a new game between Sam and Dave\n"
                 "  annotate --auto          read a game state, then start up in brief mode\n"
                 "  annotate --verify        same as --auto, but error out on any illegal move\n"
                 "  annotate --blunders      same as --auto, but error out on any bad-looking move\n"
                 "  annotate --seed 42 ...   seed the random number generator");
    }

    g_History.setup(initialState);

    for (int attacker = 0; 1; attacker = 1-attacker) {
        const bool keep_going = move_and_record(attacker);
        if (!keep_going) {
            break;
        }
        if (g_Verbose) {
            /* Did this player's move put the other player "in check"? */
            const GameState &st = g_History.currentState();
            if (!st.gameIsOver()) {
                if (findWinningMove(st, attacker, nullptr)) {
                    if (g_History.can_undo()) {
                        printf("(%s has put %s in check.)\n", g_playerNames[attacker].c_str(),
                            g_playerNames[1-attacker].c_str());
                    } else {
                        printf("(%s is in check.)\n", g_playerNames[1-attacker].c_str());
                    }
                }
            }
        }
    }

    if (g_Verbose) {
        char *filename = nullptr;
      get_filename:
        printf("Enter a filename to save a transcript to, or <return> to quit: > "); fflush(stdout);
        char *UNUSED(result) = getline_113(&filename);
        assert(result == filename || result == nullptr);
        /* result may be nullptr if end-of-file was encountered just now */
        if (filename != nullptr && filename[0] != '\0') {
            FILE *out = fopen(filename, "w");
            if (out == nullptr) {
                printf("File \"%s\" could not be opened for writing.\n", filename);
                free(filename);
                goto get_filename;
            } else {
                g_History.review(out, /*verbose=*/false);
                fclose(out);
                printf("A transcript up to this point has been saved to file \"%s\"\n", filename);
                free(filename);
            }
        } else {
            puts("No transcript was saved.");
        }
    }
    if (g_Verbose) {
        puts("Goodbye...");
    }
    return 0;
}
