
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

static bool scan_piece(const char *&text, Piece *piece)
{
    piece->color = UNKNOWN_COLOR;
    piece->size = UNKNOWN_SIZE;
    switch (*text) {
        case 'r': piece->color = RED; ++text; break;
        case 'y': piece->color = YELLOW; ++text; break;
        case 'g': piece->color = GREEN; ++text; break;
        case 'b': piece->color = BLUE; ++text; break;
        default: break;
    }
    if (piece->color != UNKNOWN_COLOR && isalpha(*text)) {
        /* don't accept "green" as a piece */
        piece->color = UNKNOWN_COLOR;
        --text;
        return false;
    }
    switch (*text) {
        case '1': piece->size = SMALL; ++text; break;
        case '2': piece->size = MEDIUM; ++text; break;
        case '3': piece->size = LARGE; ++text; break;
        default: break;
    }
    return (piece->color != UNKNOWN_COLOR || piece->size != UNKNOWN_SIZE);
}

bool SingleAction::sanitycheck() const
{
    switch (kind) {
        case SACRIFICE:
            if (!newpiece.empty() || !whither.empty()) return false;
            break;
        case CATASTROPHE:
            if (piece.size != UNKNOWN_SIZE || !newpiece.empty() || !whither.empty()) return false;
            break;
        case CAPTURE:
            if (!newpiece.empty() || !whither.empty()) return false;
            break;
        case MOVE:
            if (!newpiece.empty()) return false;
            break;
        case MOVE_CREATE:
            break;
        case BUILD:
            if (!newpiece.empty() || !whither.empty()) return false;
            break;
        case CONVERT:
            if (newpiece.size != piece.size || !whither.empty()) return false;
            break;
        default: return false;
    }
    if (!piece.sanitycheck()) return false;
    if (!newpiece.sanitycheck()) return false;
    if (!where.empty()) {
        if (!StarSystem::isValidName(where.c_str())) return false;
    }
    if (!whither.empty()) {
        if (!StarSystem::isValidName(whither.c_str())) return false;
    }
    return true;
}

bool SingleAction::isMissingPieces() const
{
    assert(this->sanitycheck());
    switch (kind) {
        case CATASTROPHE: return (piece.color == UNKNOWN_COLOR || where.empty());
        case SACRIFICE: return (piece.isMissingPieces() || where.empty());
        case CAPTURE: return (piece.isMissingPieces() || where.empty());
        case MOVE: return (piece.isMissingPieces() || where.empty() || whither.empty());
        case MOVE_CREATE: return (piece.isMissingPieces() || newpiece.isMissingPieces() || where.empty() || whither.empty());
        case BUILD: return (piece.isMissingPieces() || where.empty());
        case CONVERT: return (piece.isMissingPieces() || newpiece.isMissingPieces() || where.empty());
    }
    assert(false);
    return true;
}

bool SingleAction::scan(const char *text)
{
    this->where = ""; /* don't know */
    this->whither = ""; /* don't know */
    this->piece = Piece(UNKNOWN_COLOR, UNKNOWN_SIZE);
    this->newpiece = Piece(UNKNOWN_COLOR, UNKNOWN_SIZE);

    auto get_trailing_where = [&](auto... prepositions) {
        if (advance_past(text, prepositions...)) {
            if (!StarSystem::isValidName(text)) return false;
            this->where = text;
            return true;
        }
        return false;
    };

    if (advance_past(text, "catastrophe", "cat")) {
        this->kind = CATASTROPHE;
        // Unfortunately, because of SDG syntax, we must
        // handle all of the following:
        // "catastrophe red at Blue"
        // "catastrophe Red blue"
        // "catastrophe at Red"
        // "catastrophe red"
        // "catastrophe"
        auto get_word = [](const char **text) {
            const char *endwhere = *text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            return std::string(std::exchange(*text, endwhere), endwhere);
        };

        auto scan_color = [](const std::string& word, Piece *piece) {
            if (word == "red" || word == "r") {
                piece->color = RED;
            } else if (word == "yellow" || word == "y") {
                piece->color = YELLOW;
            } else if (word == "green" || word == "g") {
                piece->color = GREEN;
            } else if (word == "blue" || word == "b") {
                piece->color = BLUE;
            } else {
                return false;
            }
            return true;
        };

        if (*text == '\0') {
            return true;
        } else if (advance_past(text, " at ")) {
            return get_trailing_where("");
        } else if (advance_past(text, " ")) {
            // "catastrophe red [at Blue]"
            // or "catastrophe Blue red"
            std::string word1 = get_word(&text);
            if (*text == '\0') {
                return scan_color(word1, &this->piece);
            }
            ++text;
            std::string word2 = get_word(&text);
            if (word2 == "at") {
                if (*text == '\0') return false;
                if (!scan_color(word1, &this->piece)) return false;
                return get_trailing_where(" ");
            } else if (*text == '\0') {
                this->where = std::move(word1);
                if (!StarSystem::isValidName(this->where.c_str())) return false;
                return scan_color(word2, &this->piece);
            }
        }
        return false;
    } else if (advance_past(text, "sacrifice", "sac")) {
        this->kind = SACRIFICE;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, &this->piece)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where("at ", "");
    } else if (advance_past(text, "capture", "attack")) {
        this->kind = CAPTURE;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, &this->piece)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where("at ", "");
    } else if (advance_past(text, "move ")) {
        this->kind = MOVE;
        // Because of SDG syntax, we must
        // handle all of the following:
        // "move r1 from A to B (y1)"
        // "move r1 from A to B"
        // "move r1 to B (y1)"
        // "move r1 to B"
        // "move r1 A B"
        bool saw_non_sdg_syntax = false;
        if (!scan_piece(text, &this->piece)) return false;
        this->where = "";
        if (!advance_past(text, " ")) return false;
        if (advance_past(text, "from ")) {
            saw_non_sdg_syntax = true;
            const char *endwhere = text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            where = std::string(text, endwhere);
            if (!StarSystem::isValidName(this->where.c_str())) return false;
            text = endwhere;
            if (!advance_past(text, " to ")) return false;
        } else if (advance_past(text, "to ")) {
            saw_non_sdg_syntax = true;
        } else {
            const char *endwhere = text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            where = std::string(text, endwhere);
            if (!StarSystem::isValidName(this->where.c_str())) return false;
            text = endwhere;
            if (!advance_past(text, " ")) return false;
        }
        const char *endwhither = text;
        while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
        whither = std::string(text, endwhither);
        if (!StarSystem::isValidName(this->whither.c_str())) return false;
        text = endwhither;
        if (saw_non_sdg_syntax && advance_past(text, " (")) {
            // The "whither" may be a newly created star system, in which
            // case it will be followed by a piece in parentheses; for
            // example, "move y1 from Homeworld to Alpha (r2)". */
            this->kind = MOVE_CREATE;
            if (!scan_piece(text, &this->newpiece)) return false;
            if (!advance_past(text, ")")) return false;
        }
        if (*text != '\0') return false;
        return true;
    } else if (advance_past(text, "discover ")) {
        this->kind = MOVE_CREATE;
        if (!scan_piece(text, &this->piece)) return false;
        if (!advance_past(text, " ")) return false;
        const char *endwhere = text;
        while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
        where = std::string(text, endwhere);
        if (!StarSystem::isValidName(this->where.c_str())) return false;
        text = endwhere;
        if (!advance_past(text, " ")) return false;
        if (!scan_piece(text, &this->newpiece)) return false;
        if (!advance_past(text, " ")) return false;
        const char *endwhither = text;
        while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
        whither = std::string(text, endwhither);
        if (!StarSystem::isValidName(this->whither.c_str())) return false;
        text = endwhither;
        if (*text != '\0') return false;
        return true;
    } else if (advance_past(text, "build")) {
        this->kind = BUILD;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, &this->piece)) {
            if (*text == '\0') return true;
            if (!advance_past(text, " ")) return false;
        }
        return get_trailing_where("at ", "");
    } else if (advance_past(text, "convert", "trade", "swap")) {
        this->kind = CONVERT;
        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        if (scan_piece(text, &this->piece)) {
            if (!advance_past(text, " ")) return false;
        }
        bool got_where_already = false;
        if (advance_past(text, "at ", "in ")) {
            const char *endwhere = text;
            while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
            where = std::string(text, endwhere);
            if (!StarSystem::isValidName(this->where.c_str())) return false;
            text = endwhere;
            if (!advance_past(text, " ")) {
                return false;
            }
            got_where_already = true;
        }
        if (!advance_past(text, "to ", "for ", "")) return false;
        if (!scan_piece(text, &this->newpiece)) return false;
        if (this->piece.size == UNKNOWN_SIZE) {
            this->piece.size = this->newpiece.size;
        } else if (this->newpiece.size == UNKNOWN_SIZE) {
            this->newpiece.size = this->piece.size;
        } else if (this->newpiece.size != this->piece.size) {
            return false;
        }
        if (got_where_already) {
            assert(!this->where.empty());
            return (*text == '\0');
        }

        if (*text == '\0') return true;
        if (!advance_past(text, " ")) return false;
        return get_trailing_where("at ", "");
    } else {
        return false;
    }
}

bool SingleAction::scan_for_multibuild(const char *text, std::vector<SingleAction> &actions)
{
    Piece p1, p2, p3;
    const char *where;
    if (!advance_past(text, "build ")) {
        return false;
    }
    const bool got_first = scan_piece(text, &p1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, &p2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, &p3);
    if (*text == '\0') {
        where = "";
    } else if (advance_past(text, " at ", " in ", " ")) {
        if (!StarSystem::isValidName(text)) return false;
        where = text;
    } else {
        return false;
    }
    actions.push_back(SingleAction(BUILD, p1, where));
    actions.push_back(SingleAction(BUILD, p2, where));
    if (got_third) {
        actions.push_back(SingleAction(BUILD, p3, where));
    }
    return true;
}

bool SingleAction::scan_for_multicapture(const char *text, std::vector<SingleAction> &actions)
{
    Piece p1, p2, p3;
    const char *where;
    if (!advance_past(text, "capture ", "attack ")) {
        return false;
    }
    const bool got_first = scan_piece(text, &p1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, &p2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, &p3);
    if (*text == '\0') {
        where = "";
    } else if (advance_past(text, " at ", " in ", " ")) {
        if (!StarSystem::isValidName(text)) return false;
        where = text;
    } else {
        return false;
    }
    actions.push_back(SingleAction(CAPTURE, p1, where));
    actions.push_back(SingleAction(CAPTURE, p2, where));
    if (got_third) {
        actions.push_back(SingleAction(CAPTURE, p3, where));
    }
    return true;
}

bool SingleAction::scan_for_multimove(const char *text, std::vector<SingleAction> &actions)
{
    bool saw_discover = false;
    Piece p1, p2, p3, newpiece;
    std::string where;
    std::string whither;
    if (!advance_past(text, "move ")) {
        return false;
    }
    const bool got_first = scan_piece(text, &p1);
    if (!got_first) return false;
    const bool got_second = scan_piece(text, &p2);
    if (!got_second) return false;
    const bool got_third = scan_piece(text, &p3);
    where = "";
    // Because of SDG syntax, we must
    // handle all of the following:
    // "move r1 from A to B (y1)"
    // "move r1 from A to B"
    // "move r1 to B (y1)"
    // "move r1 to B"
    // "move r1 A B"
    bool saw_non_sdg_syntax = false;
    if (!advance_past(text, " ")) return false;
    if (advance_past(text, "from ")) {
        saw_non_sdg_syntax = true;
        const char *endwhere = text;
        while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
        where = std::string(text, endwhere);
        if (!StarSystem::isValidName(where.c_str())) return false;
        text = endwhere;
        if (!advance_past(text, " to ")) return false;
    } else if (advance_past(text, "to ")) {
        saw_non_sdg_syntax = true;
    } else {
        const char *endwhere = text;
        while (*endwhere != '\0' && *endwhere != ' ') ++endwhere;
        where = std::string(text, endwhere);
        if (!StarSystem::isValidName(where.c_str())) return false;
        text = endwhere;
        if (!advance_past(text, " ")) return false;
    }
    const char *endwhither = text;
    while (*endwhither != '\0' && *endwhither != ' ') ++endwhither;
    whither = std::string(text, endwhither);
    if (!StarSystem::isValidName(whither.c_str())) return false;
    text = endwhither;
    if (saw_non_sdg_syntax && advance_past(text, " (")) {
        // The "whither" may be a newly created star system, in which
        // case it will be followed by a piece in parentheses; for
        // example, "move y1 from Homeworld to Alpha (r2)". */
        saw_discover = true;
        if (!scan_piece(text, &newpiece)) return false;
        if (!advance_past(text, ")")) return false;
    }
    if (*text != '\0') return false;

    if (saw_discover) {
        actions.push_back(SingleAction(MOVE_CREATE, p1, where.c_str(), whither.c_str(), newpiece));
        actions.push_back(SingleAction(MOVE, p2, where.c_str(), whither.c_str()));
        if (got_third) {
            actions.push_back(SingleAction(MOVE, p3, where.c_str(), whither.c_str()));
        }
    } else {
        actions.push_back(SingleAction(MOVE, p1, where.c_str(), whither.c_str()));
        actions.push_back(SingleAction(MOVE, p2, where.c_str(), whither.c_str()));
        if (got_third) {
            actions.push_back(SingleAction(MOVE, p3, where.c_str(), whither.c_str()));
        }
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
                OPT(" ", color2str(piece.color)), OPT(" at ", where.c_str()));
        case SACRIFICE:
            return mprintf("sacrifice%s%s%s%s",
                OPT(" ", piece.toString()), OPT(" at ", where.c_str()));
        case CAPTURE:
            return mprintf("capture%s%s%s%s",
                OPT(" ", piece.toString()), OPT(" at ", where.c_str()));
        case MOVE:
            return mprintf("move%s%s%s%s to %s",
                OPT(" ", piece.toString()), OPT(" from ", where.c_str()), whither.c_str());
        case MOVE_CREATE:
            return mprintf("move%s%s%s%s to %s (%s)",
                OPT(" ", piece.toString()), OPT(" from ", where.c_str()), whither.c_str(),
                newpiece.toString());
        case BUILD:
            return mprintf("build%s%s%s%s",
                OPT(" ", piece.toString()), OPT(" at ", where.c_str()));
        case CONVERT:
            return mprintf("convert%s%s to %s%s%s",
                OPT(" ", piece.toString()), newpiece.toString(),
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
                where.c_str(), color2str(piece.color));
        case SACRIFICE:
            return mprintf("sacrifice %s %s",
                piece.toString(), where.c_str());
        case CAPTURE:
            return mprintf("attack %s %s",
                piece.toString(), where.c_str());
        case MOVE:
            return mprintf("move %s %s %s",
                piece.toString(), where.c_str(), whither.c_str());
        case MOVE_CREATE:
            return mprintf("discover %s %s %s %s",
                piece.toString(), where.c_str(),
                newpiece.toString(), whither.c_str());
        case BUILD:
            return mprintf("build %s %s",
                piece.toString(), where.c_str());
        case CONVERT:
            return mprintf("trade %s %s %s",
                piece.toString(), newpiece.toString(), where.c_str());
        default:
            assert(false);
    }
    /*NOTREACHED*/
}
