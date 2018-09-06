
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#include "state.h"
#include "move.h"

template <typename T>
class FreeOnReturn {
    T &ptr;
public:
    FreeOnReturn(T &p): ptr(p) { }
    ~FreeOnReturn() { free(ptr); }
};


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

static bool scan_for_multibuild(const char *text, std::vector<SingleAction> &actions)
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

static bool scan_for_multicapture(const char *text, std::vector<SingleAction> &actions)
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

static bool scan_for_multimove(const char *text, std::vector<SingleAction> &actions)
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
        std::string just_the_where(text, endwhere);
        where = just_the_where;
        if (!StarSystem::is_valid_name(where.c_str())) return false;
        text = endwhere;
    }
    if (!advance_past(text, " to ")) {
        return false;
    }
    const char *endwhither = text;
    while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
    std::string just_the_whither(text, endwhither);
    whither = just_the_whither;
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
            std::string just_the_where(text, endwhere);
            where = just_the_where;
            if (!StarSystem::is_valid_name(this->where.c_str())) return false;
            text = endwhere;
        }
        if (!advance_past(text, " to ")) return false;
        const char *endwhither = text;
        while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
        std::string just_the_whither(text, endwhither);
        whither = just_the_whither;
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
            std::string just_the_where(text, endwhere);
            where = std::move(just_the_where);
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
                OPT(" ", piece2str(color, size)), OPT(" at ", where.c_str()));
        case CAPTURE:
            return mprintf("capture%s%s%s%s",
                 OPT(" ", piece2str(color, size)), OPT(" at ", where.c_str()));
        case MOVE:
            return mprintf("move%s%s%s%s to %s",
                OPT(" ", piece2str(color, size)), OPT(" from ", where.c_str()), whither.c_str());
        case MOVE_CREATE:
            return mprintf("move%s%s%s%s to %s (%s)",
                OPT(" ", piece2str(color, size)), OPT(" from ", where.c_str()), whither.c_str(),
                piece2str(newcolor, newsize));
        case BUILD:
            return mprintf("build%s%s%s%s",
                OPT(" ", piece2str(color, size)), OPT(" at ", where.c_str()));
        case CONVERT:
            return mprintf("convert%s%s to %s%s%s",
                OPT(" ", piece2str(color, size)), piece2str(newcolor, size),
                OPT(" at ", where.c_str()));
        default:
            assert(false);
    }
    /*NOTREACHED*/
}



/* A WholeMove is valid only if it consists of a possibly empty
 * sequence of catastrophes, followed by an optional single sacrifice,
 * followed by an appropriate number of regular actions, and finished
 * with another possibly empty sequence of catastrophes. */
bool WholeMove::sanitycheck() const
{
    int i = 0;
    int n = actions.size();
    while (i < n && actions[i].kind == CATASTROPHE) {
        if (!actions[i].sanitycheck()) return false;
        ++i;
    }
    if (i == n) {
        return true;
    }

    if (actions[i].kind == SACRIFICE) {
        if (!actions[i].sanitycheck()) return false;
        Color sac_color = actions[i].color;
        Size sac_size = actions[i].size;
        int entitled_actions = (sac_size == UNKNOWN_SIZE) ? 3 : (1+(int)actions[i].size);
        int last_entitled_action_idx = i + entitled_actions;
        ++i;
        /* All the actions from here up to the final sequence
         * of catastrophes must be of the proper color. */
        while (i < n && actions[i].kind != CATASTROPHE) {
            if (!actions[i].sanitycheck()) return false;
            switch (actions[i].kind) {
                case CATASTROPHE: assert(false); return false;
                case SACRIFICE: return false;
                case CAPTURE: if (sac_color != UNKNOWN_COLOR && sac_color != RED) return false; break;
                case MOVE: if (sac_color != UNKNOWN_COLOR && sac_color != YELLOW) return false; break;
                case MOVE_CREATE: if (sac_color != UNKNOWN_COLOR && sac_color != YELLOW) return false; break;
                case BUILD: if (sac_color != UNKNOWN_COLOR && sac_color != GREEN) return false; break;
                case CONVERT: if (sac_color != UNKNOWN_COLOR && sac_color != BLUE) return false; break;
                default: assert(false); return false;
            }
            ++i;
        }
        /* You can't take more actions than you sacrificed for. */
        if (i > last_entitled_action_idx+1) return false;
    } else {
        /* A single free action. */
        if (!actions[i].sanitycheck()) return false;
        ++i;
    }
    /* The move may end with a sequence of catastrophes. */
    while (i < n && actions[i].kind == CATASTROPHE) {
        ++i;
    }
    return (i == n);
}


bool WholeMove::is_missing_pieces() const
{
    for (int i=0; i < (int)actions.size(); ++i) {
        if (actions[i].is_missing_pieces()) return true;
    }
    return false;
}


bool WholeMove::scan(const char *text)
{
    actions.clear();
    if (*text == '\0') {
        /* Safety catch: An empty string does not mean "pass". */
        return false;
    } else if (!strcmp(text, "pass")) {
        assert(this->isPass());
        return true;
    }
    char *buffer = nullptr;
    FreeOnReturn<char *> fr(buffer);
    const char *semicolon;
    while (*text != '\0') {
        bool success;
        semicolon = strchr(text, ';');
        if (semicolon == nullptr) {
            success = scan_for_multibuild(text, actions)
                   || scan_for_multicapture(text, actions)
                   || scan_for_multimove(text, actions);
            if (!success) {
                actions.push_back(SingleAction());
                success = actions.back().scan(text);
                if (!success) return false;
            }
            break;
        } else {
            if (semicolon[1] != ' ') return false;
            buffer = (char *)realloc(buffer, semicolon - text + 1);
            assert(buffer != nullptr);
            memcpy(buffer, text, semicolon - text);
            buffer[semicolon - text] = '\0';
            success = scan_for_multibuild(buffer, actions)
                   || scan_for_multicapture(buffer, actions)
                   || scan_for_multimove(buffer, actions);
            if (!success) {
                actions.push_back(SingleAction());
                success = actions.back().scan(buffer);
                if (!success) return false;
            }
            text = semicolon+2;
        }
    }
    return sanitycheck();
}


std::string WholeMove::toString() const
{
    assert(this->sanitycheck());
    if (this->isPass()) {
        return "pass";
    } else {
        std::string result = "";
        for (int i=0; i < (int)actions.size(); ++i) {
            if (i > 0) result += "; ";
            result += actions[i].toString();
        }
        return result;
    }
}
