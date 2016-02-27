
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
#include <sys/stat.h>
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
#include "ApplyMove.h"
#include "InferMove.h"
#include "PlanetNames.h"

#define randint0(n) (rand() % (n))
#define do_crash do_error

static std::string g_playerNames[2];
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
    void showState(FILE *fp) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose)
          fprintf(fp, "The current state is:\n");
        fputs(colorize(hvec[hidx].st.toString()).c_str(), fp);
    }
    void showStash(FILE *fp) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (g_Verbose)
          fprintf(fp, "The current stash is:\n");
        fprintf(fp, "%s\n", colorize(hvec[hidx].st.stash.toString()).c_str());
    }
    void review(FILE *fp, bool verbose) const {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (verbose && hidx > 0)
          fprintf(fp, "This game has gone on for %d moves.\n", hidx);
        fprintf(fp, "%s", hvec[0].st.toString().c_str());
        for (int i=1; i <= hidx; ++i) {
            if (verbose)
              fprintf(fp, "%d. %s: ", i, g_playerNames[1-(i%2)].c_str());
            fprintf(fp, "%s\n", hvec[i].move.toString().c_str());
        }
        if (verbose && hidx > 0)
          showState(fp);
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
                puts(hvec[hidx].st.toString().c_str());
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
    puts("  ai_move");
    puts("  color, mono");
    puts("  rate_moves, count_moves");
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
    printf("%s, set up your homeworld.\n", g_playerNames[attacker].c_str());

    assert(StarSystem::is_valid_name(g_playerNames[attacker].c_str()));
    st.stars.push_back(StarSystem(g_playerNames[attacker].c_str()));
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


/* The special command "LEGAL build r1" runs findAllMoves()
 * and verifies that "build r1" indeed appears in the list.
 * That's how we validate that findAllMoves() could be used
 * by a GUI program to basically diff any pair of states into
 * a legal move. */
static void verify_move(bool legal, const WholeMove &move, int attacker)
{
    if (ApplyMove::isValidMove(g_History.currentstate(), attacker, move)) {
        if (!legal)
          printf("Failed isValidMove test: %sLEGAL %s\n", (legal ? "" : "IL"), move.toString().c_str());
        std::vector<WholeMove> allmoves;
        findAllMoves(g_History.currentstate(), attacker, allmoves,
                /*prune_obviously_worse_moves=*/false,
                /*look_only_for_wins=*/false,
                /*these_colors_only=*/0xF);
        GameState targetst = g_History.currentstate();
        ApplyMove::or_die(targetst, attacker, move);
        const std::string target = targetst.toComparableString();
        bool found = false;
        for (int i=0; i < (int)allmoves.size(); ++i) {
            GameState newst = g_History.currentstate();
            ApplyMove::or_die(newst, attacker, allmoves[i]);
            if (newst.toComparableString() == target) {
                found = true;
                break;
            }
        }
        if (found != legal)
          printf("Failed findAllMoves test: %sLEGAL %s\n", (legal ? "" : "IL"), move.toString().c_str());
        if (legal && targetst.hasLost(1-attacker)) {
            /* The given move is supposed to be legal AND winning;
             * therefore findWinningMove() should return true. */
            if (!findWinningMove(g_History.currentstate(), attacker, NULL))
              printf("Failed findWinningMove test: LEGAL %s\n", move.toString().c_str());
        }
    } else {
        if (legal) {
            printf("Failed isValidMove test: %sLEGAL %s\n", (legal ? "" : "IL"), move.toString().c_str());
            printf("Skipping findAllMoves test\n");
            printf("Skipping findWinningMove test\n");
        }
    }
}


static void make_move_and_report(int attacker, const WholeMove& move)
{
    if (g_ReportBlunders && move_was_boneheaded(g_History.currentstate(), move, attacker)) {
        printf("%s blundered into check on this move:\n%s\n",
            g_playerNames[attacker].c_str(), move.toString().c_str());
        printf("The position was:\n");
        printf("%s\n", g_History.currentstate().toString().c_str());
    }

    g_History.makemove(move, attacker);

    bool game_is_over = g_History.currentstate().gameIsOver();
    if (game_is_over) {
        if (g_Verbose) {
            printf("%s has won the game!\n", g_playerNames[attacker].c_str());
            puts("(Valid commands at this point include \"review\" and \"help\".)");
        }
    }

    if (g_Verbose && !game_is_over && g_History.currentstate().containsOverpopulation()) {
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
    const bool game_is_over = g_History.currentstate().gameIsOver();
    char *moveline_cstr = NULL;
    std::string moveline;

    while (true) {
        /* Prompt for the human's move, and read in a line. */
        if (g_Verbose) {
            printf("%s's move? > ", g_playerNames[attacker].c_str()); fflush(stdout);
        }
        char *result = getline_113(&moveline_cstr);
        assert(result == moveline_cstr || result == NULL);
        /* Act as if there's an implicit "quit" at the end of the input;
         * otherwise, we'd just go on reading the last command forever. */
        if (result == NULL) {
            moveline = "quit";
        } else {
            moveline = moveline_cstr;
            free(moveline_cstr);
        }

        if (moveline == "help") {
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
        } else if (moveline == "color") {
            if (g_Verbose) {
                puts("The output will now be colorized.");
                puts("To return to monochrome mode, enter \"mono\".");
            }
            g_Colorize = true;
        } else if (moveline == "mono") {
            if (g_Verbose) {
                puts("The output will no longer be colorized.");
            }
            g_Colorize = false;
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
            if (out == NULL) {
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
            if (g_VerifyTranscript) do_error("Transcript is incorrect.");
        } else if (moveline == "rate_moves") {
            std::vector<WholeMove> allmoves;
            get_all_moves_sorted_by_value(g_History.currentstate(), attacker, allmoves, true);
        } else if (moveline == "count_moves") {
            std::vector<WholeMove> allmoves;
            findAllMoves(g_History.currentstate(), attacker, allmoves,
                    /*prune=*/false, /*wins=*/false, /*colors=*/0xf);
            assert(allmoves.size() >= 1);  /* "pass" from a legal position is always legal */
            printf("%d\n", (int)allmoves.size());
        } else if (!strncmp(moveline.c_str(), "LEGAL ", 6) ||
                   !strncmp(moveline.c_str(), "ILLEGAL ", 8) ||
                   !strncmp(moveline.c_str(), "AMBIG ", 6)) {
            const bool checkLegal = !strncmp(moveline.c_str(), "LEGAL ", 6);
            const bool checkIllegal = !strncmp(moveline.c_str(), "ILLEGAL ", 8);
            const bool checkAmbig = !strncmp(moveline.c_str(), "AMBIG ", 6);
            const char * const realmoveline = (checkIllegal ? &moveline[8] : &moveline[6]);
            WholeMove move;
            const bool success = move.scan(realmoveline);
            if (!success) {
                printf("Failed: \"%s\" didn't parse as a move\n", realmoveline);
            } else {
                WholeMove oldmove = move;
                const bool inferred = move.is_missing_pieces() ?
                    inferMoveFromState(g_History.currentstate(), attacker, move) : true;
                if (checkAmbig == inferred) {
                    printf("Failed: \"%s\" is%s ambiguous.\n",
                        oldmove.toString().c_str(), checkAmbig ? " not" : "");
                }
                if (inferred && !checkAmbig) {
                    verify_move(checkLegal, move, attacker);
                }
            }
        } else {
            WholeMove move;
            const bool isAiMove = (moveline == "ai_move");
            const bool isWinMove = (moveline == "WIN");
            if (isAiMove) {
                move = get_ai_move(g_History.currentstate(), attacker);
                if (g_Verbose) {
                    printf("AI for %s chooses: %s\n", g_playerNames[attacker].c_str(), move.toString().c_str());
                }
                assert(ApplyMove::isValidMove(g_History.currentstate(), attacker, move));
                make_move_and_report(attacker, move);
                return true;
            } else if (isWinMove) {
                const bool success = findWinningMove(g_History.currentstate(), attacker, &move);
                if (!success) {
                    printf("AI for %s found no winning move.\n", g_playerNames[attacker].c_str());
                } else {
                    if (g_Verbose) {
                        printf("AI for %s found a winning move:\n", g_playerNames[attacker].c_str());
                        printf("%s\n", move.toString().c_str());
                    }
                    assert(ApplyMove::isValidMove(g_History.currentstate(), attacker, move));
                    make_move_and_report(attacker, move);
                    return true;
                }
            } else {
                const bool success = move.scan(moveline.c_str());
                if (!success) {
                    if (strlen(moveline.c_str()) > 0) {
                        puts("The given string did not parse as a move. It was:");
                        printf("\"%s\"\n", moveline.c_str());
                    }
                    if (g_VerifyTranscript) do_error("Transcript is incorrect.");
                    puts("Enter the string \"help\" for help with this game's interface.");
                } else {
                    if (move.is_missing_pieces()) {
                        WholeMove oldmove = move;
                        const bool inferred = inferMoveFromState(g_History.currentstate(), attacker, move);
                        if (!inferred) {
                            /* We couldn't infer the user's intended move. Just restore the old move,
                             * with the un-filled-in blanks, and let isValidMove() reject it below. */
                            move = oldmove;
                        }
                    }
                    /* If we've gotten this far, the user (or AI) gave us a syntactically
                     * correct move. Try to apply it; if it's semantically invalid or
                     * illegal, reject it. */
                    const bool success = ApplyMove::isValidMove(g_History.currentstate(), attacker, move);
                    if (!success) {
                        puts("The move as parsed was invalid, ambiguous, or disallowed by the rules. It was:");
                        printf("\"%s\"\n", move.toString().c_str());
                        if (g_VerifyTranscript) do_error("Transcript is incorrect.");
                        puts("Enter the string \"help\" for help with this game's interface.");
                    } else {
                        /* We got a completely valid move. */
                        if (g_Verbose) {
                            puts("Okay.");
                        }
                        make_move_and_report(attacker, move);
                        return true;
                    }
                }
            }
        }
    }
}

int main(int argc, char **argv) {

    srand((unsigned int)time(NULL));

    int arg_index;
    bool auto_setup = false;
    bool load_setup = false;
    char *file_name;
    for (arg_index=1; arg_index < argc; ++arg_index) {
        if (argv[arg_index][0] != '-') break;
        std::string arg = argv[arg_index];
        if (arg == "--") { ++arg_index; break; }
        if (arg == "--blunders") {
            g_ReportBlunders = true;
        } else if (arg == "--verify") {
            g_VerifyTranscript = true;
        } else if (arg == "--auto") {
            auto_setup = true;
        } else if (arg == "--seed") {
            if (arg_index+1 >= argc || !isdigit(argv[arg_index+1][0]))
              do_error("The --seed argument requires an integer parameter!");
            ++arg_index;
            srand((unsigned int)atoi(argv[arg_index]));
        } else if (arg == "--load") {
            if (arg_index+1 >= argc)
              do_error("The -- load argument requires a valid file transcript to read!");
            ++arg_index;
            file_name = argv[arg_index];
            load_setup = true;
        } else {
            do_error("Unrecognized command-line argument %s", argv[arg_index]);
        }
    }

    if (g_ReportBlunders || g_VerifyTranscript)
      auto_setup = true;

    GameState initialState;

    if (load_setup) {
        struct stat buf;
        if (stat(file_name, &buf) != 0) {
            do_error("The loading file doesn't exist.");
        }
        g_Verbose = true;
        FILE * file = fopen(file_name, "r");
        std::string firstline = initialState.scan(file);
        fclose(file);
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
        g_playerNames[0] = hw->name;
        hw = initialState.homeworldOf(1);
        if (hw == NULL)
          do_error("The initial homeworld setup didn't include Player 1's homeworld!");
        g_playerNames[1] = hw->name;
    } else if (auto_setup && arg_index == argc) {
        /* "annotate --auto" means that the input will be in the form of a
         * game transcript, and we should be quiet instead of verbose. */
        g_Verbose = false;
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
        g_playerNames[0] = hw->name;
        hw = initialState.homeworldOf(1);
        if (hw == NULL)
          do_error("The initial homeworld setup didn't include Player 1's homeworld!");
        g_playerNames[1] = hw->name;
    } else if (!auto_setup && arg_index+2 == argc) {
        /* "annotate Sam Dave" means that the input will be entered via the
         * keyboard as the game progresses, and we should be verbose
         * (acknowledging valid moves, prompting the user to re-enter invalid
         * moves, et cetera). */
        g_Verbose = true;
        if (!StarSystem::is_valid_name(argv[arg_index])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[arg_index]);
        } else if (!StarSystem::is_valid_name(argv[arg_index+1])) {
            do_error("Sorry, the argument \"%s\" was not a valid name for a star system.", argv[arg_index+1]);
        }

        g_playerNames[0] = argv[arg_index];
        g_playerNames[1] = argv[arg_index+1];
        initialState.newGame();

        printf("%s will set up first and move first.\n", g_playerNames[0].c_str());
        setup_human(initialState, 0);

        printf("The state after %s's setup is:\n", g_playerNames[0].c_str());
        printf("%s\n", initialState.toString().c_str());
        setup_human(initialState, 1);

        printf("The state after both players' setup is:\n");
        printf("%s\n", initialState.toString().c_str());
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
        if (!keep_going)
          break;
        if (g_Verbose) {
            /* Did this player's move put the other player "in check"? */
            const GameState &st = g_History.currentstate();
            if (!st.gameIsOver()) {
                if (findWinningMove(st, attacker, NULL)) {
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
        char *filename = NULL;
      get_filename:
        printf("Enter a filename to save a transcript to, or <return> to quit: > "); fflush(stdout);
        char *UNUSED(result) = getline_113(&filename);
        assert(result == filename || result == NULL);
        /* result may be NULL if end-of-file was encountered just now */
        if (filename != NULL && filename[0] != '\0') {
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
