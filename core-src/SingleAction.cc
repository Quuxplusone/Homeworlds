
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#include "SingleAction.h"
#include "mprintf.h"
#include "state.h"

bool SingleAction::getAssociatedColor(Color *color) const {
    switch (kind) {
        case SACRIFICE:
        case CATASTROPHE:
            return false;
        case CAPTURE:
            *color = RED;
            return true;
        case MOVE:
        case MOVE_CREATE:
            *color = YELLOW;
            return true;
        case BUILD:
            *color = GREEN;
            return true;
        case CONVERT:
            *color = BLUE;
            return true;
    }
}

template<class... ConstCharPtr>
static bool advance_past(const char *&text, ConstCharPtr... prefixes)
{
    for (const char *prefix : {prefixes...}) {
        if (strncmp(text, prefix, strlen(prefix)) == 0) {
            text += strlen(prefix);
            return true;
        }
    }
    return false;
}

static bool scan_piece(const char *&text, Color &color, Size &size)
{
    color = UNKNOWN_COLOR;
    size = UNKNOWN_SIZE;
    switch (*text) {
        case 'r': color = RED; ++text; break;
        case 'y': color = YELLOW; ++text; break;
        case 'g': color = GREEN; ++text; break;
        case 'b': color = BLUE; ++text; break;
        default: break;
    }
    if (color != UNKNOWN_COLOR && isalpha(*text)) {
        /* don't accept "green" as a piece */
        color = UNKNOWN_COLOR;
        --text;
        return false;
    }
    switch (*text) {
        case '1': size = SMALL; ++text; break;
        case '2': size = MEDIUM; ++text; break;
        case '3': size = LARGE; ++text; break;
        default: break;
    }
    return (color != UNKNOWN_COLOR || size != UNKNOWN_SIZE);
}

bool SingleAction::sanitycheck() const
{
    switch (kind) {
        case SACRIFICE:
        case CATASTROPHE:
        case CAPTURE:
        case MOVE:
        case MOVE_CREATE:
        case BUILD:
        case CONVERT: break;
        default: return false;
    }
    switch (color) {
        case RED: case YELLOW: case GREEN: case BLUE: break;
        case UNKNOWN_COLOR: break;
        default: return false;
    }
    if (kind != CATASTROPHE) {
        switch (size) {
            case SMALL: case MEDIUM: case LARGE: break;
            case UNKNOWN_SIZE: break;
            default: return false;
        }
    }
    if (kind == CONVERT || kind == MOVE_CREATE) {
        switch (newcolor) {
            case RED: case YELLOW: case GREEN: case BLUE: break;
            case UNKNOWN_COLOR: break;
            default: return false;
        }
    }
    if (kind == MOVE_CREATE) {
        switch (newsize) {
            case SMALL: case MEDIUM: case LARGE: break;
            case UNKNOWN_SIZE: break;
            default: return false;
        }
    }
    if (where != "" && !StarSystem::is_valid_name(where.c_str())) {
        return false;
    }
    if (kind == MOVE || kind == MOVE_CREATE) {
        if (whither != "" && !StarSystem::is_valid_name(whither.c_str())) {
            return false;
        }
    }
    return true;
}

bool SingleAction::is_missing_pieces() const
{
    assert(this->sanitycheck());
    if (color == UNKNOWN_COLOR) return true;
    if (kind != CATASTROPHE && size == UNKNOWN_SIZE) return true;
    if (kind == CONVERT || kind == MOVE_CREATE) {
        if (newcolor == UNKNOWN_COLOR) return true;
    }
    if (kind == MOVE_CREATE && newsize == UNKNOWN_SIZE) return true;
    if (where == "") return true;
    if (kind == MOVE || kind == MOVE_CREATE) {
        if (whither == "") return true;
    }
    return false;
}

bool SingleAction::scan(const char *text)
{
    this->color = UNKNOWN_COLOR;
    this->size = UNKNOWN_SIZE;
    this->where = ""; /* don't know */
    this->newcolor = UNKNOWN_COLOR;
    this->newsize = UNKNOWN_SIZE;
    this->whither = ""; /* don't know */

    auto get_trailing_where = [&]() {
        if (advance_past(text, "at ", "in ", "")) {
            if (!StarSystem::is_valid_name(text)) return false;
            this->where = text;
            return true;
        }
        return false;
    };

    if (advance_past(text, "catastrophe", "cat")) {
        this->kind = CATASTROPHE;
        if (advance_past(text, " red", " r")) {
            this->color = RED;
        } else if (advance_past(text, " yellow", " y")) {
            this->color = YELLOW;
        } else if (advance_past(text, " green", " g")) {
            this->color = GREEN;
        } else if (advance_past(text, " blue", " b")) {
            this->color = BLUE;
        }
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        return get_trailing_where();
    } else if (advance_past(text, "sacrifice", "sac")) {
        this->kind = SACRIFICE;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, this->color, this->size)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where();
    } else if (advance_past(text, "capture", "attack")) {
        this->kind = CAPTURE;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, this->color, this->size)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where();
    } else if (advance_past(text, "move ")) {
        this->kind = MOVE;
        if (!scan_piece(text, this->color, this->size)) return false;
        this->where = "";
        if (advance_past(text, " from ")) {
            const char *endwhere = text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            where = std::string(text, endwhere);
            if (!StarSystem::is_valid_name(this->where.c_str())) return false;
            text = endwhere;
        }
        if (!advance_past(text, " to ")) return false;
        const char *endwhither = text;
        while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
        whither = std::string(text, endwhither);
        if (!StarSystem::is_valid_name(this->whither.c_str())) return false;
        text = endwhither;
        /* The "whither" may be a newly created star system, in which
         * case it will be followed by a piece in parentheses; for
         * example, "move y1 from Homeworld to Alpha (r2)". */
        if (*text == '\0') return true;
        this->kind = MOVE_CREATE;
        if (!advance_past(text, " (")) return false;
        if (!scan_piece(text, this->newcolor, this->newsize)) return false;
        if (!advance_past(text, ")")) return false;
        if (*text != '\0') return false;
        return true;
    } else if (advance_past(text, "discover ")) {
        this->kind = MOVE_CREATE;
        if (!scan_piece(text, this->color, this->size)) return false;
        if (!advance_past(text, " ")) return false;
        const char *endwhere = text;
        while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
        where = std::string(text, endwhere);
        if (!StarSystem::is_valid_name(this->where.c_str())) return false;
        text = endwhere;
        if (!advance_past(text, " ")) return false;
        if (!scan_piece(text, this->newcolor, this->newsize)) return false;
        if (!advance_past(text, " ")) return false;
        const char *endwhither = text;
        while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
        whither = std::string(text, endwhither);
        if (!StarSystem::is_valid_name(this->whither.c_str())) return false;
        text = endwhither;
        if (*text != '\0') return false;
        return true;
    } else if (advance_past(text, "build")) {
        this->kind = BUILD;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, this->color, this->size)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where();
    } else if (advance_past(text, "convert", "trade", "swap")) {
        this->kind = CONVERT;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, this->color, this->size)) {
            if (!advance_past(text, " ")) return false;
        }
        bool got_where_already = false;
        if (advance_past(text, "at ", "in ")) {
            const char *endwhere = text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            where = std::string(text, endwhere);
            if (!StarSystem::is_valid_name(this->where.c_str())) return false;
            text = endwhere;
            if (!advance_past(text, " ")) {
                return false;
            }
            got_where_already = true;
        }
        if (!advance_past(text, "to ", "for ", "")) return false;
        if (!scan_piece(text, this->newcolor, this->newsize)) return false;
        if (this->size == UNKNOWN_SIZE) {
            this->size = this->newsize;
        } else if (this->newsize == UNKNOWN_SIZE) {
            this->newsize = this->size;
        } else if (this->newsize != this->size) {
            return false;
        }
        if (got_where_already) {
            assert(!this->where.empty());
            return (*text == '\0');
        }

        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        return get_trailing_where();
    } else {
        return false;
    }
}

bool SingleAction::scan_for_multibuild(const char *text, std::vector<SingleAction> &actions)
{
    Color c1, c2, c3;
    Size s1, s2, s3;
    const char *where;
    if (!advance_past(text, "build ")) {
        return false;
    }
    const bool got_first = scan_piece(text, c1, s1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, c2, s2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, c3, s3);
    if (*text == '\0') {
        where = "";
    } else if (advance_past(text, " at ", " in ", " ")) {
        if (!StarSystem::is_valid_name(text)) return false;
        where = text;
    } else {
        return false;
    }
    actions.push_back(SingleAction(BUILD, c1, s1, where));
    actions.push_back(SingleAction(BUILD, c2, s2, where));
    if (got_third) {
        actions.push_back(SingleAction(BUILD, c3, s3, where));
    }
    return true;
}

bool SingleAction::scan_for_multicapture(const char *text, std::vector<SingleAction> &actions)
{
    Color c1, c2, c3;
    Size s1, s2, s3;
    const char *where;
    if (!advance_past(text, "capture ", "attack ")) {
        return false;
    }
    const bool got_first = scan_piece(text, c1, s1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, c2, s2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, c3, s3);
    if (*text == '\0') {
        where = "";
    } else if (advance_past(text, " at ", " in ", " ")) {
        if (!StarSystem::is_valid_name(text)) return false;
        where = text;
    } else {
        return false;
    }
    actions.push_back(SingleAction(CAPTURE, c1, s1, where));
    actions.push_back(SingleAction(CAPTURE, c2, s2, where));
    if (got_third) {
        actions.push_back(SingleAction(CAPTURE, c3, s3, where));
    }
    return true;
}

bool SingleAction::scan_for_multimove(const char *text, std::vector<SingleAction> &actions)
{
    Color c1, c2, c3;
    Size s1, s2, s3;
    std::string where;
    std::string whither;
    if (!advance_past(text, "move ")) {
        return false;
    }
    const bool got_first = scan_piece(text, c1, s1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, c2, s2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, c3, s3);
    where = "";
    if (advance_past(text, " from ")) {
        const char *endwhere = text;
        while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
        where = std::string(text, endwhere);
        if (!StarSystem::is_valid_name(where.c_str())) return false;
        text = endwhere;
    }
    if (!advance_past(text, " to ")) {
        return false;
    }
    const char *endwhither = text;
    while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
    whither = std::string(text, endwhither);
    if (!StarSystem::is_valid_name(whither.c_str())) return false;
    text = endwhither;
    /* The "whither" may be a newly created star system, in which
     * case it will be followed by a piece in parentheses; for
     * example, "move y1 from Homeworld to Alpha (r2)". */
    if (*text == '\0') {
        actions.push_back(SingleAction(MOVE, c1, s1, where.c_str(), whither.c_str()));
    } else {
        Color newcolor;
        Size newsize;
        if (!advance_past(text, " (")) return false;
        if (!scan_piece(text, newcolor, newsize)) return false;
        if (!advance_past(text, ")")) return false;
        if (*text != '\0') return false;
        actions.push_back(SingleAction(MOVE_CREATE, c1, s1, where.c_str(), whither.c_str(), newcolor, newsize));
    }
    actions.push_back(SingleAction(MOVE, c2, s2, where.c_str(), whither.c_str()));
    if (got_third) {
        actions.push_back(SingleAction(MOVE, c3, s3, where.c_str(), whither.c_str()));
    }
    return true;
}

#define OPT(prefix, str)  (str[0] ? prefix : ""), (str)

std::string SingleAction::toString() const
{
    assert(this->sanitycheck());
    switch (kind) {
        case CATASTROPHE:
            return mprintf("catastrophe%s%s%s%s",
                OPT(" ", color2str(color)), OPT(" at ", where.c_str()));
        case SACRIFICE:
            return mprintf("sacrifice%s%s%s%s",
                OPT(" ", Piece(color, size).toString()), OPT(" at ", where.c_str()));
        case CAPTURE:
            return mprintf("capture%s%s%s%s",
                OPT(" ", Piece(color, size).toString()), OPT(" at ", where.c_str()));
        case MOVE:
            return mprintf("move%s%s%s%s to %s",
                OPT(" ", Piece(color, size).toString()), OPT(" from ", where.c_str()), whither.c_str());
        case MOVE_CREATE:
            return mprintf("move%s%s%s%s to %s (%s)",
                OPT(" ", Piece(color, size).toString()), OPT(" from ", where.c_str()), whither.c_str(),
                Piece(newcolor, newsize).toString());
        case BUILD:
            return mprintf("build%s%s%s%s",
                OPT(" ", Piece(color, size).toString()), OPT(" at ", where.c_str()));
        case CONVERT:
            return mprintf("convert%s%s to %s%s%s",
                OPT(" ", Piece(color, size).toString()), Piece(newcolor, size).toString(),
                OPT(" at ", where.c_str()));
        default:
            assert(false);
    }
    /*NOTREACHED*/
}

std::string SingleAction::toSDGString() const
{
    assert(this->sanitycheck());
    switch (kind) {
        case CATASTROPHE:
            return mprintf("catastrophe %s %s",
                where.c_str(), color2str(color));
        case SACRIFICE:
            return mprintf("sacrifice %s %s",
                Piece(color, size).toString(), where.c_str());
        case CAPTURE:
            return mprintf("attack %s %s",
                Piece(color, size).toString(), where.c_str());
        case MOVE:
            return mprintf("move %s %s %s",
                Piece(color, size).toString(), where.c_str(), whither.c_str());
        case MOVE_CREATE:
            return mprintf("discover %s %s %s %s",
                Piece(color, size).toString(), where.c_str(),
                Piece(newcolor, newsize).toString(), whither.c_str());
        case BUILD:
            return mprintf("build %s %s",
                Piece(color, size).toString(), where.c_str());
        case CONVERT:
            return mprintf("trade %s %s %s",
                Piece(color, size).toString(), Piece(newcolor, size).toString(), where.c_str());
        default:
            assert(false);
    }
    /*NOTREACHED*/
}
