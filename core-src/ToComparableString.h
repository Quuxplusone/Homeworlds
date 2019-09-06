#pragma once

#include <assert.h>
#include <algorithm>
#include <string>
#include <vector>

inline char *PieceCollection::toComparableString(char buffer[MAXSTRLEN+1], unsigned char mask) const
{
    const signed char *ptr = &pieces[0][0];
    for (int i=0; i < 12; ++i) {
        for (int j=0; j < ptr[i]; ++j) {
            *buffer++ = 'A' + i + mask;
        }
    }
    return buffer;
}

/* Return a string describing this star system's pieces in an unambiguous
 * way. It doesn't matter whether this is someone's homeworld, since that's
 * handled in GameState::toComparableString(). */
inline char *StarSystem::toComparableString(char buffer[MAXSTRLEN+1]) const
{
    /* This routine will have to be adjusted if NUMPLAYERS ever increases. */
    static_assert(NUMPLAYERS == 2, "");
    char *bp = buffer;
    bp = star.toComparableString(bp, /*mask=*/0x00);
    bp = ships[0].toComparableString(bp, /*mask=*/0x10);
    bp = ships[1].toComparableString(bp, /*mask=*/0x20);
    return bp;
}

inline std::string StarSystem::toComparableString() const
{
    char buffer[MAXSTRLEN+1];
    char *bp = this->toComparableString(buffer);
    return std::string(buffer, bp);
}

inline std::string GameState::toComparableString() const
{
    /* Put the homeworlds first, because they are "special" regardless of
     * their position in the galaxy or their names. */
    char homeworlds[NUMPLAYERS*StarSystem::MAXSTRLEN + 1];
    char *bp = homeworlds;
    for (int i=0; i < NUMPLAYERS; ++i) {
        const StarSystem *hw = homeworldOf(i);
        if (hw != nullptr) {
            bp = hw->toComparableString(bp);
        } else {
            *bp++ = '!';
        }
    }
    /* For non-homeworld stars, the only thing that matters is the piece
     * distribution of the star and ships; name and position don't matter.
     * So use StarSystem::toComparableString() to throw out the name,
     * and sort the results to get rid of position information. */
    std::string result(homeworlds, bp);
    std::string v[21];
    size_t vn = 0;
    for (size_t i=0; i < stars.size(); ++i) {
        if (stars[i].homeworldOf == -1) {
            char buffer[StarSystem::MAXSTRLEN+1];
            char *bp = stars[i].toComparableString(buffer);
            v[vn++].assign(buffer, bp);
        }
    }
    std::sort(v, v + vn);
    for (size_t i=0; i < vn; ++i) {
        result += v[i];
    }
    return result;
}
