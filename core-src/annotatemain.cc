
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
#include "move.h"
#include "AI.h"
#include "AllMoves.h"
#include "AlphaBeta.h"
#include "ApplyMove.h"
#include "InferMove.h"
#include "PlanetNames.h"

#define randint0(n) (rand() % (n))
#define do_crash do_error

static char *g_playerNames[2];
static bool g_Verbose;
static bool g_ReportBlunders;
static bool g_VerifyTranscript;
static bool g_Colorize;


/* A string consists of alphanumeric words separated by punctuation and
 * whitespace. If a word looks like a valid PieceCollection (according to
 * PieceCollection::scan()), then colorize it accordingly.
 */
static std::string colorize(const std::string &texts)
{
    if (!g_Colorize)
	return texts;
    const char *text = texts.c_str();
    std::string result;
    while (*text != '\0') {
	if (isalnum(*text)) {
	    const char *start = text;
	    while (isalnum(*text)) ++text;
	    std::string candidate(start, text);
	    PieceCollection pc;
	    if (pc.scan(candidate.c_str())) {
		const char *cp = candidate.c_str();
		char current_color = 'x';
		while (*cp != '\0') {
		    switch (*cp) {
			case '1': case '2': case '3': break;
			case 'r': if (current_color != 'r') result += "\33[31m"; current_color = 'r'; break;
			case 'y': if (current_color != 'y') result += "\33[33m"; current_color = 'y'; break;
			case 'g': if (current_color != 'g') result += "\33[32m"; current_color = 'g'; break;
			case 'b': if (current_color != 'b') result += "\33[34m"; current_color = 'b'; break;
			default: assert(false);
		    }
		    result += *cp;
		    ++cp;
		}
		assert(current_color != 'x');
		result += "\33[0m";
	    } else {
		result += candidate;
	    }
	} else {
	    result += *text;
	    ++text;
	}
    }
    return result;
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
    void setup(const GameState &st) {
        assert(hidx == 0);
        assert(hvec.size() == 1);
        hvec[0].st = st;
    }
    void showState(FILE *fp) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose)
          fprintf(fp, "The current state is:\n");
        fputs(colorize(hvec[hidx].st.toString()).c_str(), fp);
    }
    void showStash(FILE *fp) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose)
          fprintf(fp, "The current stash is:\n");
        fprintf(fp, "%s\n", colorize(hvec[hidx].st.stash.toString()).c_str());
    }
    void review(FILE *fp, bool verbose) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (verbose && hidx > 0)
          fprintf(fp, "This game has gone on for %d moves.\n", hidx);
        fprintf(fp, "%s", hvec[0].st.toString().c_str());
        for (int i=1; i <= hidx; ++i) {
            if (verbose)
              fprintf(fp, "%d. %s: ", i, g_playerNames[1-(i%2)]);
            fprintf(fp, "%s\n", hvec[i].move.toString().c_str());
        }
        if (verbose && hidx > 0)
          showState(fp);
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
                puts(hvec[hidx].st.toString().c_str());
            }
            return true;
        }
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
                printf("%d. %s: %s\n", hidx, g_playerNames[1-(hidx%2)],
                    hvec[hidx].move.toString().c_str());
                puts("The move has been redone. The current state is:");
                puts(hvec[hidx].st.toString().c_str());
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
        const bool UNUSED(success) = ApplyMove::Whole(hvec[hidx].st, attacker, move);
        assert(success);
        hvec[hidx].move = move;
    }
    const GameState &currentstate() const {
        return hvec[hidx].st;
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
    puts("  rollout <branching-factor-at-ply-0> ...");
    puts("  ai_move");
    puts("  deepmove <branching-factor> <time-limit-in-seconds>");
    puts("  quit");
    puts("The \"review\" command prints a transcript of the game up to this point,");
    puts("or writes the same transcript to the specified text file.");
    puts("The \"state\" command prints the current state of the game.");
    puts("The \"ai_move\" command instructs the computer to pick a reasonable move for");
    puts("the current player, and then have the current player make that move.");
    puts("The \"rollout\" command instructs the computer to look for the best possible");
    puts("move from this position, searching the specified number of ply deep in the");
    puts("game tree with a maximum of \"branching-factor\" moves considered per ply.");
    puts("A \"branching-factor\" of 0 does an exhaustive search of all moves per ply.");
    puts("\"rollout 1 0\" is equivalent to \"ai_move\", except that the move is not");
    puts("automatically made.");
    puts("The \"hint\" command is equivalent to \"rollout\", except that it does not");
    puts("reveal the best move; it searches to the specified depth in the game tree and");
    puts("reports only whether the current position is a guaranteed win or loss for the");
    puts("current player.");
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
        if (opponent_hw != NULL) {
            /* Make sure the homeworlds are as far apart as possible.
             * This condition disallows (r1y1, g2b3) and (r1y2, g1b2)
             * while still allowing (r1y1, g1b2) and (r1y2, g1b3). */
            if (opponent_hw->star.numberOf(s1) == opponent_hw->star.numberOf(s2))
              continue;
        }
        /* The initial ship should be big and non-red. If the star doesn't have green
         * already, then the initial ship must be green. Otherwise, the ship must be
         * of a color that's not already in the star itself. */
        const Color shipc = (c2 != GREEN) ? GREEN : (c1 == YELLOW) ? BLUE : YELLOW;
        hw.ships[attacker].insert(shipc, LARGE);
        /* If this configuration isn't actually possible, rinse and repeat. */
        if (!st.stash.contains(hw.pieceCollection()))
          continue;
        /* This configuration is okay. */
        break;
    }
    st.stash -= hw.star;
    st.stash -= hw.ships[attacker];
    return;
}


static void setup_human(GameState &st, int attacker)
{
    printf("%s, set up your homeworld.\n", g_playerNames[attacker]);

    assert(StarSystem::is_valid_name(g_playerNames[attacker]));
    st.stars.push_back(StarSystem(g_playerNames[attacker]));
    StarSystem &hw = st.stars.back();
    hw.homeworldOf = attacker;

    char *moveline = NULL;
    char *result;
    printf("Enter the initial pieces for your star. For example, \"y3b2\". > "); fflush(stdout);
  get_star_pieces:
    result = getline_113(&moveline);
    /* Ignore input errors for the sake of simplicity. TODO FIXME BUG HACK */
    assert(result == moveline && result != NULL);
    if (strcmp(moveline, "ai_move") == 0) {
        free(moveline);
        printf("AI is setting up a homeworld for %s...\n", g_playerNames[attacker]);
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
        printf("%s\n", st.stash.toString().c_str());
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
    assert(result == moveline && result != NULL);
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
        printf("%s\n", st.stash.toString().c_str());
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
static bool move_was_boneheaded(const GameState &oldst, const WholeMove &m, int attacker)
{
    assert(!oldst.gameIsOver());
    GameState newst = oldst;
    ApplyMove::or_die(newst, attacker, m);
    if (newst.gameIsOver())
      return false;

    if (!findWinningMove(newst, 1-attacker, NULL))
      return false;
    /* The attacker did move into check.
     * But maybe he didn't have any choice? */
    std::vector<WholeMove> allmoves;
    findAllMoves_usualcase(oldst, attacker, allmoves);
    for (int i=0; i < (int)allmoves.size(); ++i) {
        newst = oldst;
        ApplyMove::or_die(newst, attacker, allmoves[i]);
        /* If he'd made this move instead, would he still be in check? */
        if (newst.gameIsOver()) {
            /* No, in fact he'd have won the game! */
            return true;
        }
	if (!findWinningMove(newst, 1-attacker, NULL)) {
            /* No, he wouldn't still be in check. */
            return true;
        }
    }
    /* All moves led to check, so he was going to lose either way. */
    return false;
}


/* Rollout-related stuff. */
struct GameStateAugmented : public GameState {
    typedef int Value;

    int attacker;
    int turn_number;
    Value value;
    
    static time_t timelimit;
    static bool have_run_out_of_time;
    static int branching_factor[20];
    static int evaluateCount;
    
    GameStateAugmented() { }
    GameStateAugmented(const GameState &st, int a):
            GameState(st), attacker(a), turn_number(0), value(0) { }

    static Value evaluate(const GameStateAugmented &sta)
    {
        ++evaluateCount;
        return sta.value;
    }
    static void applyMove(GameStateAugmented &sta, const WholeMove &move)
    {
        ApplyMove::or_die(sta, sta.attacker, move);
        const StarSystem *hw = sta.homeworldOf(1-sta.attacker);
        /* The "value" of this position needs to be consistent; if the
         * position is valued at roughly +X on my turn, it should be valued
         * at roughly -X on your turn. It shouldn't be valued at +Y just
         * because we both like this position. Otherwise, the breadth-first
         * search applied by "deepmove" will go horribly wrong. */
        if (hw == NULL || hw->ships[1-sta.attacker].empty()) {
            sta.value = INT_MAX;
        } else if (sta.turn_number % 2 == 0) {
            sta.value = 100000 + 10000 * ai_static_evaluation(sta, sta.attacker);
        } else {
            sta.value = -sta.value + ai_static_evaluation(sta, sta.attacker);
        }
        sta.attacker = (1 - sta.attacker);
        sta.turn_number += 1;
    }
    static void findMoves(const GameStateAugmented &sta, std::vector<WholeMove> &allmoves)
    {
	if (sta.gameIsOver()) return;
	assert(allmoves.empty());
	get_all_moves_sorted_by_value(sta, sta.attacker, allmoves, false);
        assert(!allmoves.empty());
        const int turn_delta = sta.turn_number;
        const int factor = branching_factor[std::min(turn_delta, 19)];
        if (factor != 0 && (int)allmoves.size() > factor)
          allmoves.resize(factor);
    }
    static void findMovesTimed(const GameStateAugmented &sta, std::vector<WholeMove> &allmoves)
    {
        /* If we have run out of time, then pretend that the search has bottomed out. */
        if (have_run_out_of_time) return;
        if (time(NULL) >= timelimit) {
            printf("Pencils down. (searched to %d ply)\n", sta.turn_number);
            have_run_out_of_time = true;
            return;
        }
	if (sta.gameIsOver()) return;
	assert(allmoves.empty());
	get_all_moves_sorted_by_value(sta, sta.attacker, allmoves, false);
        assert(!allmoves.empty());
        if ((int)allmoves.size() > branching_factor[0])
          allmoves.resize(branching_factor[0]);
    }
    static int findAttacker(const GameStateAugmented &sta)
    {
        return sta.attacker;
    }
};
time_t GameStateAugmented::timelimit;
bool GameStateAugmented::have_run_out_of_time;
int GameStateAugmented::branching_factor[20];
int GameStateAugmented::evaluateCount;

typedef GameStateAugmented GSA;
static AlphaBeta<GameStateAugmented, WholeMove, GSA::Value>
    g_RolloutAB(GSA::evaluate, GSA::applyMove, GSA::findMoves, GSA::findAttacker);
static AlphaBeta<GameStateAugmented, WholeMove, GSA::Value>
    g_TimedSearchAB(GSA::evaluate, GSA::applyMove, GSA::findMovesTimed, GSA::findAttacker);


static bool move_and_record(int attacker)
{
    const bool game_is_over = g_History.currentstate().gameIsOver();
    char *moveline = NULL;
  get_move:
    /* Prompt for the human's move, and read in a line. */
    if (g_Verbose) {
        printf("%s's move? > ", g_playerNames[attacker]); fflush(stdout);
    }
    char *result = getline_113(&moveline);
    assert(result == moveline || result == NULL);
    /* Act as if there's an implicit "quit" at the end of the input;
     * otherwise, we'd just go on reading the last command forever. */
    if (result == NULL)
      strcpy(moveline, "quit");
    
    if (strcmp(moveline, "help") == 0) {
        free(moveline);
        do_help();
        goto get_move;
    } else if (strcmp(moveline, "quit") == 0) {
        free(moveline);
        return false;
    } else if (strcmp(moveline, "verbose") == 0) {
        free(moveline);
        puts("Now operating in verbose mode.");
        g_Verbose = true;
        goto get_move;
    } else if (strcmp(moveline, "brief") == 0) {
        free(moveline);
        if (g_Verbose) {
            puts("Now operating in non-verbose \"brief\" mode.");
            puts("No transcript will be saved if you \"quit\" while in brief mode!");
        }
        g_Verbose = false;
        goto get_move;
    } else if (strcmp(moveline, "color") == 0) {
        free(moveline);
	if (g_Verbose) {
	    puts("The output will now be colorized.");
	    puts("To return to monochrome mode, enter \"mono\".");
	}
        g_Colorize = true;
        goto get_move;
    } else if (strcmp(moveline, "mono") == 0) {
        free(moveline);
	if (g_Verbose)
	    puts("The output will no longer be colorized.");
        g_Colorize = false;
        goto get_move;
    } else if (moveline[0] == '#') {
        /* This is a comment in a transcript file; ignore it. */
        free(moveline);
        goto get_move;
    } else if (strcmp(moveline, "undo") == 0) {
        free(moveline);
        const bool success = g_History.undo();
        if (success) return true;
        goto get_move;
    } else if (strcmp(moveline, "redo") == 0) {
        free(moveline);
        const bool success = g_History.redo();
        if (success) return true;
        goto get_move;
    } else if (strcmp(moveline, "review") == 0) {
        free(moveline);
        g_History.review(stdout, g_Verbose);
        goto get_move;
    } else if (strcmp(moveline, "state") == 0) {
        free(moveline);
        g_History.showState(stdout);
        goto get_move;
    } else if (strcmp(moveline, "stash") == 0) {
        free(moveline);
        g_History.showStash(stdout);
        goto get_move;
    } else if (strncmp(moveline, "review ", 7) == 0) {
        const char *filename = moveline+7;
        FILE *out = fopen(filename, "w");
        if (out == NULL) {
            printf("File \"%s\" could not be opened for writing.\n", filename);
        } else {
            g_History.review(out, /*verbose=*/false);
            fclose(out);
            if (g_Verbose)
              printf("A transcript up to this point has been saved to file \"%s\".\n", filename);
        }
        free(moveline);
        goto get_move;
    } else if (game_is_over) {
        /* Once the game is over you're not allowed to continue making
         * moves, but you can still use the bookkeeping commands above
         * this check. */
        if (g_Verbose) {
            puts("The game is over; only \"undo\" and \"review\" are allowed\n"
                   "at this point. You entered:");
            printf("\"%s\"\n", moveline);
            puts("Enter the string \"help\" for help with this game's interface.\n");
        } else {
            puts("The game is already over!");
        }
        if (g_VerifyTranscript) do_error("Transcript is incorrect.");
        free(moveline);
        goto get_move;
    } else if (!strncmp(moveline, "rollout ", 8)) {
        int ply = 0;
        char *ptr = moveline + 8;
        char *endptr;
        bool success = true;
        while (success && *ptr != '\0') {
            while (*ptr == ' ') ++ptr;
            if (*ptr == '\0')
              break;
            if (ply >= 20) {
                success = false;
                break;
            }
            GSA::branching_factor[ply] = strtol(ptr, &endptr, 10);
            if (endptr == ptr)
              success = false;
            if (GSA::branching_factor[ply] < 0 || GSA::branching_factor[ply] >= 100000)
              success = false;
            ptr = endptr;
            ++ply;
        }
        if (!success) {
            puts("The \"rollout\" command expects between 1 and 20 arguments,");
            puts("each of which must be a branching factor between 1 and 100000, or 0.");
            printf("You entered: \"%s\"\n", moveline);
        } else {
            assert(1 <= ply && ply <= 20);
            GameStateAugmented sta(g_History.currentstate(), attacker);
            WholeMove bestmove;
            GSA::Value bestvalue = 42;  /* initialize just to avoid a warning */
            const bool UNUSED(has_moves) =
                g_RolloutAB.depth_first_alpha_beta(sta, ply, bestmove, bestvalue, -1000, +1000);
            assert(has_moves);
            reassignPlanetNames(bestmove, g_History.currentstate(), NULL);
            if (bestvalue == +1000) {
                if (g_Verbose)
                  printf("The position is a sure win for %s.\n", g_playerNames[attacker]);
                printf("-> %s\n", bestmove.toString().c_str());
            } else if (bestvalue == -1000) {
                if (g_Verbose)
                  printf("The position is a sure loss for %s.\n", g_playerNames[attacker]);
                puts("-> pass");
            } else {
                if (g_Verbose) {
                    printf("The position is neither a sure win nor a sure loss for %s.\n",
                        g_playerNames[attacker]);
                }
                printf("-> %s\n", bestmove.toString().c_str());
            }
        }
        free(moveline);
        goto get_move;
    } else if (!strncmp(moveline, "deepmove ", 9)) {
        char *ptr = moveline + 9;
        char *endptr;
        bool success = true;
        GSA::branching_factor[0] = strtol(ptr, &endptr, 10);
        if (endptr == ptr || GSA::branching_factor[0] <= 0 || GSA::branching_factor[0] >= 100000)
          success = false;
        ptr = endptr;
        int seconds = strtol(ptr, &endptr, 10);
        if (endptr == ptr || seconds <= 0 || seconds > 3600) {
            success = false;
        } else {
            GSA::timelimit = (time(NULL) + seconds);
            GSA::have_run_out_of_time = false;
        }
        if (!success) {
            puts("The \"deepmove\" command expects two arguments:");
            puts("a branching factor and a time limit in seconds.");
            printf("You entered: \"%s\"\n", moveline);
            free(moveline);
            goto get_move;
        } else {
            free(moveline);
            GameStateAugmented sta(g_History.currentstate(), attacker);
            WholeMove bestmove;
            GSA::Value bestvalue;
            GSA::evaluateCount = 0;
            const bool UNUSED(has_moves) = g_TimedSearchAB.breadth_first(sta, INT_MAX, bestmove, bestvalue);
            assert(has_moves);
            reassignPlanetNames(bestmove, g_History.currentstate(), NULL);
            assert(ApplyMove::isValidMove(g_History.currentstate(), attacker, bestmove));
            printf("(evaluated %d leaf positions)\n", GSA::evaluateCount);
            if (bestvalue == +1000) {
                if (g_Verbose)
                  printf("The position is a sure win for %s.\n", g_playerNames[attacker]);
                printf("AI chooses: %s\n", bestmove.toString().c_str());
            } else if (bestvalue == -1000) {
                if (g_Verbose)
                  printf("The position is a sure loss for %s.\n", g_playerNames[attacker]);
                puts("AI chooses: pass");
            } else {
                printf("AI for %s chooses: %s\n", g_playerNames[attacker],
                    bestmove.toString().c_str());
            }
            g_History.makemove(bestmove, attacker);
            if (g_History.currentstate().gameIsOver()) {
                if (g_Verbose) {
                    printf("%s has won the game!\n", g_playerNames[attacker]);
                    puts("(Valid commands at this point include \"review\" and \"help\".)");
                }
            }
            return true;
        }
    } else if (strcmp(moveline, "rate_moves") == 0) {
        free(moveline);
        std::vector<WholeMove> allmoves;
        get_all_moves_sorted_by_value(g_History.currentstate(), attacker, allmoves, true);
        goto get_move;
    } else {
        WholeMove move;
        const bool isAiMove = !strcmp(moveline, "ai_move");
        if (isAiMove) {
            move = get_ai_move(g_History.currentstate(), attacker);
            if (g_Verbose)
              printf("AI for %s chooses: %s\n", g_playerNames[attacker], move.toString().c_str());
        } else {
            const bool success = move.scan(moveline);
            if (!success) {
                puts("The given string did not parse as a move. It was:");
                printf("\"%s\"\n", moveline);
                if (g_VerifyTranscript) do_error("Transcript is incorrect.");
                puts("Enter the string \"help\" for help with this game's interface.");
                free(moveline);
                goto get_move;
            }
            if (move.is_missing_pieces()) {
                WholeMove oldmove = move;
                const bool inferred = inferMoveFromState(g_History.currentstate(), attacker, move);
                if (!inferred) {
                    /* We couldn't infer the user's intended move. Just restore the old move,
                     * with the un-filled-in blanks, and let isValidMove() reject it below. */
                    move = oldmove;
                }
            }
        }
        free(moveline);
        /* If we've gotten this far, the user (or AI) gave us a syntactically
         * correct move. Try to apply it; if it's semantically invalid or
         * illegal, reject it. */
        if (isAiMove) {
            assert(ApplyMove::isValidMove(g_History.currentstate(), attacker, move));
        } else {
            const bool success = ApplyMove::isValidMove(g_History.currentstate(), attacker, move);
            if (!success) {
                puts("The move as parsed was invalid, ambiguous, or disallowed by the rules. It was:");
                printf("\"%s\"\n", move.toString().c_str());
                if (g_VerifyTranscript) do_error("Transcript is incorrect.");
                puts("Enter the string \"help\" for help with this game's interface.");
                goto get_move;
            }
        }
        /* We got a completely valid move. */
        if (g_Verbose && !isAiMove)
          puts("Okay.");
        if (g_ReportBlunders && move_was_boneheaded(g_History.currentstate(), move, attacker)) {
            printf("%s blundered into check on this move:\n%s\n",
                g_playerNames[attacker], move.toString().c_str());
             printf("The position was:\n");
            printf("%s\n", g_History.currentstate().toString().c_str());
        }
        g_History.makemove(move, attacker);
        if (g_History.currentstate().gameIsOver()) {
            if (g_Verbose) {
                printf("%s has won the game!\n", g_playerNames[attacker]);
                puts("(Valid commands at this point include \"review\" and \"help\".)");
            }
        }
        return true;
    }
}


int main(int argc, char **argv)
{
    srand((unsigned int)time(NULL));

    GameState initialState;

    if (argc == 2 && (!strcmp(argv[1], "-auto") || !strcmp(argv[1], "-blunders") || !strcmp(argv[1], "-verify"))) {
        /* "annotate -auto" means that the input will be in the form of a
         * game transcript, and we should be quiet instead of verbose. */
        g_Verbose = false;
	g_ReportBlunders = !strcmp(argv[1], "-blunders");
	g_VerifyTranscript = !strcmp(argv[1], "-verify");
	std::string firstline = initialState.scan(stdin);
	firstline += "\n";
	for (int i = firstline.length(); i > 0; --i) {
	    int rc = ungetc(firstline[i-1], stdin);
	    if (rc == EOF) {
	        do_error("ungetc: %d character%s of input could not be pushed back.\n"
	            "Try adding a blank line after the homeworld setup lines.", i,
	            (i>1 ? "s" : ""));
	    }
	}
	assignPlanetNames(initialState, NULL);
	StarSystem *hw = initialState.homeworldOf(0);
	if (hw == NULL)
          do_error("The initial homeworld setup didn't include Player 0's homeworld!");
        g_playerNames[0] = new char[strlen(hw->name.c_str())+1];
        strcpy(g_playerNames[0], hw->name.c_str());
	hw = initialState.homeworldOf(1);
	if (hw == NULL)
          do_error("The initial homeworld setup didn't include Player 1's homeworld!");
        g_playerNames[1] = new char[strlen(hw->name.c_str())+1];
        strcpy(g_playerNames[1], hw->name.c_str());
    } else {
        /* "annotate Sam Dave" means that the input will be entered via the
         * keyboard as the game progresses, and we should be verbose
         * (acknowledging valid moves, prompting the user to re-enter invalid
         * moves, et cetera). */
        g_Verbose = true;
        if (argc != 3) {
            do_error("Program requires two arguments: the names of the first player\n"
                     "to set up and the first player to move, respectively.");
        } else if (!StarSystem::is_valid_name(argv[1])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[1]);
        } else if (!StarSystem::is_valid_name(argv[2])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[2]);
        }

        g_playerNames[0] = argv[1];
        g_playerNames[1] = argv[2];
        initialState.newGame();

        /* The player who sets up first will move second; the player who
         * sets up second will move first. */
        printf("%s will set up first and move first.\n", g_playerNames[0]);
        setup_human(initialState, 0);

        printf("The state after %s's setup is:\n", g_playerNames[0]);
        printf("%s\n", initialState.toString().c_str());
        setup_human(initialState, 1);

        printf("The state after both players' setup is:\n");
        printf("%s\n", initialState.toString().c_str());
    }

    g_History.setup(initialState);

    for (int attacker = 0; 1; attacker = 1-attacker) {
        const bool keep_going = move_and_record(attacker);
        if (!keep_going)
          break;
        if (g_Verbose) {
            /* Did this player's move put the other player "in check"? */
            const GameState &st = g_History.currentstate();
            if (!st.gameIsOver()) {
                if (findWinningMove(st, attacker, NULL)) {
                    printf("(%s has put %s in check.)\n", g_playerNames[attacker],
                        g_playerNames[1-attacker]);
                }
            }
        }
    }
    
    if (g_Verbose) {
        char *filename = NULL;
      get_filename:
        printf("Enter a filename to save a transcript to, or <return> to quit: > "); fflush(stdout);
        char *UNUSED(result) = getline_113(&filename);
        /* Ignore input errors for the sake of simplicity. TODO FIXME BUG HACK */
        assert(result == filename && result != NULL);
        if (filename[0] != '\0') {
            FILE *out = fopen(filename, "w");
            if (out == NULL) {
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
    if (g_Verbose)
      puts("Goodbye...");
    return 0;
}

