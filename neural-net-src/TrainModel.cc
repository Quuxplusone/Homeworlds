
#include <assert.h>
#include <random>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "getline.h"
#include <algorithm>
#include <set>
#include <string>
#include <vector>
#include "state.h"
#include "move.h"
#include "ApplyMove.h"
#include "AutoEncoderTrainer.h"
#include "EvaluatorTrainer.h"
#include "GameStateEncoder.h"
#include "History.h"
#include "Net.h"
#include "State2Vec.h"

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


static bool move_and_record(FILE *fp, int attacker)
{
    const bool game_is_over = g_History.currentstate().gameIsOver();
    char *moveline_cstr = nullptr;
    std::string moveline;

    while (true) {
        /* Prompt for the human's move, and read in a line. */
        char *result = fgetline_113(&moveline_cstr, fp);
        assert(result == moveline_cstr || result == nullptr);
        /* Act as if there's an implicit "quit" at the end of the input;
         * otherwise, we'd just go on reading the last command forever. */
        if (result == nullptr) {
            return false;
        } else {
            moveline = moveline_cstr;
            free(moveline_cstr);
        }

        if (moveline == "") {
            /* do nothing */
        } else if (moveline[0] == '#') {
            /* This is a comment in a transcript file; ignore it. */
        } else if (game_is_over) {
            do_error("Transcript is incorrect.");
        } else {
            WholeMove move;
            const bool success = move.scan(moveline.c_str());
            if (!success) {
                puts("The given string did not parse as a move. It was:");
                printf("\"%s\"\n", moveline.c_str());
                do_error("Transcript is incorrect.");
            } else {
                assert(!move.is_missing_pieces());
                const bool success = ApplyMove::isValidMove(g_History.currentstate(), attacker, move);
                if (!success) {
                    puts("The move as parsed was invalid, ambiguous, or disallowed by the rules. It was:");
                    printf("\"%s\"\n", move.toString().c_str());
                    do_error("Transcript is incorrect.");
                } else {
                    /* We got a completely valid move. */
                    g_History.makemove(move, attacker);
                    return true;
                }
            }
        }
    }
}

void slurp_game(const char *fname)
{
    FILE *fp = fopen(fname, "r");
    GameState initialState;
    std::string firstline = initialState.scan(fp);
    firstline += "\n";
    for (int i = firstline.length(); i > 0; --i) {
        int rc = ungetc(firstline[i-1], fp);
        if (rc == EOF) {
            do_error("ungetc: %d character%s of input could not be pushed back.\n"
                "Try adding a blank line after the homeworld setup lines.", i,
                (i>1 ? "s" : ""));
        }
    }
    StarSystem *hw = initialState.homeworldOf(0);
    if (hw == nullptr) {
        do_error("The initial homeworld setup didn't include Player 0's homeworld!");
    }
    hw = initialState.homeworldOf(1);
    if (hw == nullptr) {
        do_error("The initial homeworld setup didn't include Player 1's homeworld!");
    }
    g_History.setup(initialState);

    for (int attacker = 0; 1; attacker = 1-attacker) {
        const bool keep_going = move_and_record(fp, attacker);
        if (!keep_going) {
            break;
        }
    }
    fclose(fp);
}

int main(int argc, char **argv)
{
    std::string command = argv[1];
    if (command == "encoder") {
        AutoEncoderStage1 ae;
        for (int i=2; i < argc; ++i) {
            slurp_game(argv[i]);
            ae.add_history_to_training_set(g_History);
        }
        ae.train();
        AutoEncoderStage2 ae2(&ae);
        ae2.train();
        AutoEncoderStage3 ae3(&ae2);
        ae3.train();
        FILE *fp = fopen("encoder_weights.txt", "w");
        ae3.save_to_file(fp);
        fclose(fp);
    } else if (command == "evaluator") {
        auto encoder = std::make_unique<GameStateEncoder>();
        FILE *fp = fopen("encoder_weights.txt", "r");
        encoder->load_from_file(fp);
        fclose(fp);

        EvaluatorTrainer e(encoder.get());
        for (int i=2; i < argc; ++i) {
            slurp_game(argv[i]);
            e.maybe_add_history_to_training_set(g_History);
        }
        e.train();
        fp = fopen("evaluator_weights.txt", "w");
        e.save_to_file(fp);
        fclose(fp);
    }
}
