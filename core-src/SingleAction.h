#pragma once

#include "PieceCollection.h"
#include "global.h"
#include <assert.h>
#include <stdio.h>
#include <string>
#include <vector>

enum SingleActionKind {
    CATASTROPHE, SACRIFICE, CAPTURE, MOVE, MOVE_CREATE, BUILD, CONVERT
};

/* A SingleAction is one of
 *     catastrophe %color at %where
 *     sacrifice %color%size at %where
 *     capture %color%size at %where
 *       [from %defender, but we needn't implement that yet]
 *     move %color%size from %where to %whither
 *       [this is the MOVE kind]
 *     move %color%size from %where to %whither (%newcolor%newsize)
 *       [this is the MOVE_CREATE kind]
 *     build %color%size at %where
 *     convert %color%size to %newcolor%size at %where
 * Various SingleActions can be combined into one WholeMove
 * using the + and += operators.
 *   Note that any field of a SingleAction (except for the "kind")
 * may be set to "don't know" (UNKNOWN_COLOR, UNKNOWN_SIZE, or
 * the empty string ""). An action with "don't know" fields cannot
 * be applied, but it can be combined with a GameState to infer the
 * values of the missing fields.
 */
class SingleAction {
public:
    bool isMissingPieces() const;
    bool getAssociatedColor(Color *color) const;

    std::string toString() const;
    std::string toSDGString() const;
    bool scan(const char *text);
    SingleAction() = default;
    SingleAction(const char *text) { const bool UNUSED(rc) = scan(text); assert(rc); }
    SingleAction(const std::string &text) : SingleAction(text.c_str()) {}

    /* These constructors are provided for efficiency. If you don't care about efficiency,
     * you should probably just use the constructor SingleAction(const char *) and let it parse
     * out the action for you. */

    // SingleAction(SACRIFICE, r1, "Sacloc")
    // SingleAction(CAPTURE, r1, "Caploc")
    // SingleAction(BUILD, r1, "Buildloc")
    explicit SingleAction(SingleActionKind k, Piece p, const char *w) :
      kind(k), where(w), piece(p)
      { assert(k == SACRIFICE || k == CAPTURE || k == BUILD); }

    // SingleAction(CATASTROPHE, RED, "Catloc")
    explicit SingleAction(SingleActionKind k, Color c, const char *w) :
      kind(k), where(w), piece(c, UNKNOWN_SIZE)
      { assert(k == CATASTROPHE); }

    // SingleAction(MOVE, r1, "Fromloc", "Toloc")
    explicit SingleAction(SingleActionKind k, Piece p, const char *w, const char *wr) :
      kind(k), where(w), whither(wr), piece(p)
      { assert(k == MOVE); }

    // SingleAction(MOVE_CREATE, r1, "Fromloc", "Toloc", b2)
    explicit SingleAction(SingleActionKind k, Piece p, const char *w, const char *wr, Piece np):
      kind(k), where(w), whither(wr), piece(p), newpiece(np)
      { assert(k == MOVE_CREATE); }

    // SingleAction(MOVE_CREATE, r1, "Fromloc", string("Toloc"), b2)
    explicit SingleAction(SingleActionKind k, Piece p, const char *w, std::string wr, Piece np):
      kind(k), where(w), whither(std::move(wr)), piece(p), newpiece(np)
      { assert(k == MOVE_CREATE); }

    // SingleAction(CONVERT, r1, b1, "Atloc")
    explicit SingleAction(SingleActionKind k, Piece p, Piece np, const char *w) :
      kind(k), where(w), piece(p), newpiece(np)
      { assert(k == CONVERT); }

    bool sanitycheck() const;

    static bool scan_for_multicapture(const char *text, std::vector<SingleAction>& actions);
    static bool scan_for_multimove(const char *text, std::vector<SingleAction>& actions);
    static bool scan_for_multibuild(const char *text, std::vector<SingleAction>& actions);

public:
    /* These fields should be treated as read-only. */
    SingleActionKind kind;
    std::string where;
    std::string whither;
    Piece piece;
    Piece newpiece;
};
