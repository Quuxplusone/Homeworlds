#pragma once

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
    bool is_missing_pieces() const;
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
    /* SingleAction(SACRIFICE, RED, SMALL, "Sacloc")
     * SingleAction(CAPTURE, RED, SMALL, "Caploc")
     * SingleAction(BUILD, RED, SMALL, "Buildloc") */
    explicit SingleAction(SingleActionKind k, Color c, Size s, const char *w):
      kind(k), where(w), color(c), size(s)
      { assert(k == SACRIFICE || k == CAPTURE || k == BUILD); }
    /* SingleAction(CATASTROPHE, RED, "Catloc") */
    explicit SingleAction(SingleActionKind k, Color c, const char *w):
      kind(k), where(w), color(c)
      { assert(k == CATASTROPHE); }
    /* SingleAction(MOVE, RED, SMALL, "Fromloc", "Toloc") */
    explicit SingleAction(SingleActionKind k, Color c, Size s, const char *w, const char *wr):
      kind(k), where(w), whither(wr), color(c), size(s)
      { assert(k == MOVE); }
    /* SingleAction(MOVE_CREATE, RED, SMALL, "Fromloc", "Toloc", BLUE, MEDIUM) */
    explicit SingleAction(SingleActionKind k, Color c, Size s, const char *w, const char *wr, Color nc, Size ns):
      kind(k), where(w), whither(wr), color(c), size(s), newcolor(nc), newsize(ns)
      { assert(k == MOVE_CREATE); }
    /* SingleAction(MOVE_CREATE, RED, SMALL, "Fromloc", string("Toloc"), BLUE, MEDIUM) */
    explicit SingleAction(SingleActionKind k, Color c, Size s, const char *w, std::string wr, Color nc, Size ns):
      kind(k), where(w), whither(std::move(wr)), color(c), size(s), newcolor(nc), newsize(ns)
      { assert(k == MOVE_CREATE); }
    /* SingleAction(CONVERT, RED, SMALL, BLUE, "Atloc") */
    explicit SingleAction(SingleActionKind k, Color c, Size s, Color nc, const char *w):
      kind(k), where(w), color(c), size(s), newcolor(nc), newsize(s)
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
    Color color;
    Size size;
    Color newcolor;
    Size newsize;
};
