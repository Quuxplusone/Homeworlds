import json
import libannotate
import logging
import re

from . import sdgbackend


def looksLikeNewGame(fields):
    return (
        fields.get('From', '') == 'admin@superdupergames.org' and
        fields.get('Subject', '') == '[SDG] You have been challenged!'
    )


def looksLikePlayerMove(fields):
    return (
        fields.get('From', '') == 'admin@superdupergames.org' and
        fields.get('Subject', '').startswith('[SDG] Homeworlds game #') and
        fields.get('TextBody', '').startswith('It is now your turn to move')
    )


def extractGameNumberFromEmail(fields):
    m = re.match(".SDG. Homeworlds game #(\d+) - It's your turn!", fields['Subject'])
    game_id_1 = m.group(1) if m else None
    m = re.match("It is now your turn to move in Homeworlds game #(\d+)[.].*", fields['TextBody'])
    game_id_2 = m.group(1) if m else None
    logging.info('Extracted game number %r from subject and %r from body', game_id_1, game_id_2)
    assert game_id_1 == game_id_2
    return int(game_id_1)


def parse_participants(line):
    m = re.match(r'Participants: ([A-Za-z0-9_-]+) \(S\), ([A-Za-z0-9_-]+) \(N\)$', line)
    if m is not None:
        return [m.group(2), m.group(1)]
    m = re.match(r'Participants: ([A-Za-z0-9_-]+) \(S\), ([A-Za-z0-9_-]+) \(N\), ([A-Za-z0-9_-]+) \(E\)$', line)
    if m is not None:
        return [m.group(2), m.group(3), m.group(1)]
    m = re.match(r'Participants: ([A-Za-z0-9_-]+) \(S\), ([A-Za-z0-9_-]+) \(W\), ([A-Za-z0-9_-]+) \(N\), ([A-Za-z0-9_-]+) \(E\)$', line)
    if m is not None:
        return [m.group(3), m.group(4), m.group(1), m.group(2)]
    assert False


def cook_header(lines):
    m = re.match(r'Homeworlds Online \(SDG# (\d+)\)$', lines[0])
    assert m is not None
    gamenumber = int(m.group(1))
    if re.match(r'Variants: ".*"$', lines[1]):
        # Ignore this line.
        lines = lines[1:]
    m = re.match(r'Started: \d+.\d+.\d+, Ended: (\d+.\d+.\d+|[?][?])$', lines[1])
    assert m is not None
    participants = parse_participants(lines[2])
    m = re.match(r'Winner: ([A-Za-z0-9_-]+|[?][?])$', lines[3])
    assert m is not None
    winner = m.group(1)
    assert (winner == '??') or (winner in participants)
    return (gamenumber, participants, lines[4:])


def colorof(c):
    if (c.title()[0] == 'R'): return 'red'
    if (c.title()[0] == 'Y'): return 'yellow'
    if (c.title()[0] == 'G'): return 'green'
    if (c.title()[0] == 'B'): return 'blue'
    assert False


def shipof(s):
    return s.lower()


def whereof(s):
    x = re.sub(r'[^a-zA-Z0-9_]', '', s)
    if (x == s) or len(x) >= 3:
        return x.title()
    x = re.sub(r'[^a-zA-Z0-9_]', '_', s)
    return x.title()


def wholemoveof(actions):
    # SDG allows catastrophes in any position; "annotate" allows them only at the end of either player's turn.
    # Games 34464, 34480 have non-leading, non-trailing catastrophes that might as well be trailing.
    # Game 33134 has a non-leading, non-trailing catastrophe that can't be either leading or trailing!
    # Rather than move the catastrophes around, let's just leave them alone and let "annotate" reject the move.
    actions = [a for a in actions if a != 'pass']
    return '; '.join(actions) or 'pass'


def reposition_catastrophes(m):
    actions = m.split('; ')
    leading = 0
    while leading < len(actions) and actions[leading].startswith('catastrophe'):
        leading += 1
    actions = (
        actions[:leading] +
        [a for a in actions[leading:] if not a.startswith('catastrophe')] +
        [a for a in actions[leading:] if a.startswith('catastrophe')]
    )
    return '; '.join(actions)


def reposition_moves(m):
    actions = m.split('; ')
    trailing = len(actions)
    while trailing > 0 and not actions[trailing - 1].startswith('move'):
        trailing -= 1
    leading = 0
    while leading < trailing and not actions[leading].startswith('move'):
        leading += 1
    actions = (
        actions[:leading] +
        list(reversed(actions[leading:trailing])) +
        actions[trailing:]
    )
    return '; '.join(actions)


def expand_regex(rx):
    # Games 21986 (1), 23891 (k), 29902 (in), 27768 (`), 31476 (!), 33526 (b), 31476 (all digits, $, !), 31476 (.), 31557 (*)
    rx = rx.replace('SHIP', r'([rygb][123])[0-9.!$*bikn`]*')
    # Game 746 (/)
    # Game 756 (horrible Unicode)
    rx = rx.replace('WHERE', r"([a-z0-9_/'-]+)")
    rx = rx.replace('COLOR', r'(red|yellow|green|blue|r|y|g|b)')
    # Game 16589 (Buil)
    rx = rx.replace('BUILD', r'(?:b|buil|build|con|construct)')
    # Game 31466 (Tr)
    rx = rx.replace('TRADE', r'(?:t|tr|trade)')
    # Game 31466 (Hom)
    rx = rx.replace('HOMEWORLD', r'(?:h|hom|home|homeworld)')
    return rx


move_regexes = {
    expand_regex(rx): then for rx, then in {
        r'^HOMEWORLD SHIP SHIP SHIP$': lambda _1, name, a, b, c: 'homeworld %s %s %s %s' % (shipof(a), shipof(b), shipof(c), whereof(name)),
        r'^HOMEWORLD SHIP SHIP SHIP WHERE$': lambda _1, name, a, b, c: 'homeworld %s %s %s %s' % (shipof(a), shipof(b), shipof(c), whereof(name)),
        r'^a(?:ttack)? SHIP[ns]? WHERE$': lambda _1, _2, a, b: 'capture %s at %s' % (shipof(a), whereof(b)),
        r'^a(?:ttack)? SHIP WHERE (?:n(?:orth)?|s(?:outh)?)$': lambda _1, _2, a, b: 'capture %s at %s' % (shipof(a), whereof(b)),
        r'^a(?:ttack)? SHIP WHERE [(][ns][)]$': lambda _1, _2, a, b: 'capture %s at %s' % (shipof(a), whereof(b)),
        r'^m(?:ove)? SHIP WHERE WHERE$': lambda _1, _2, a, b, c: 'move %s from %s to %s' % (shipof(a), whereof(b), whereof(c)),
        r'^d(?:iscover)? SHIP WHERE SHIP WHERE(?: .*)?$': lambda _1, _2, a, b, c, d: 'move %s from %s to %s (%s)' % (shipof(a), whereof(b), whereof(d), shipof(c)),
        r'^BUILD SHIP WHERE$': lambda _1, _2, a, b: 'build %s at %s' % (shipof(a), whereof(b)),
        r'^BUILD SHIP WHERE [(][ns][)]$': lambda _1, _2, a, b: 'build %s at %s' % (shipof(a), whereof(b)),
        r'^TRADE SHIP SHIP WHERE$': lambda _1, _2, a, b, c: 'convert %s to %s at %s' % (shipof(a), shipof(b), whereof(c)),
        r'^TRADE SHIP SHIP WHERE [(][ns][)]$': lambda _1, _2, a, b, c: 'convert %s to %s at %s' % (shipof(a), shipof(b), whereof(c)),
        r'^s(?:ac(?:rifice)?)? SHIP WHERE$': lambda _1, _2, a, b: 'sacrifice %s at %s' % (shipof(a), whereof(b)),
        r'^c(?:at(?:astrophe)?)? WHERE COLOR$': lambda _1, _2, a, b: 'catastrophe %s at %s' % (colorof(b), whereof(a)),
        r'^p(?:ass)?$': lambda _1, _2: 'pass',
    }.items()
}


def cook_moves(gamenumber, participants, lines):
    turn_number = 0
    turn_offset = 1
    cookedactions = []
    inside_comment = False
    for line in lines:
        action_text = None
        m = re.match(r'(\d+)[)] ([A-Za-z0-9_-]+):(.*)$', line.strip())
        if m is not None:
            inside_comment = False
            if int(m.group(1)) != turn_number + 1:
                logging.error("Surprising: turn indicator says %d but I thought it should be turn %d!" % (int(m.group(1)), turn_number + 1))
                raise RuntimeError("surprise during parsing of the raw file")
            cookedactions.append([])
            turn_number = int(m.group(1))
            attacker = (turn_number + turn_offset) % 2
            attacker_name = m.group(2)
            if whereof(attacker_name) != whereof(participants[attacker]):
                logging.error("Surprising: %s moved but I thought it was %s's turn!" % (attacker_name, participants[attacker]))
                raise RuntimeError("surprise during parsing of the raw file")
            action_text = m.group(3).strip()

            if turn_number == turn_offset + 1 and wholemoveof(cookedactions[0]) == 'pass':
                # Game 21443: If the first player passes, we just flip the player names and keep going.
                participants = [participants[1], participants[0]]
                cookedactions = [[]]
                turn_offset += 1
                attacker = (turn_number + turn_offset) % 2

        elif inside_comment:
            continue  # it's a comment from a human player
        elif line.startswith('\t'):
            inside_comment = True
            continue  # it's a comment from a human player
        else:
            action_text = line.strip()

        if action_text == '':
            # it's a blank line
            continue

        # SDG requires an asterisk on certain homeworld setups.
        # Some players put the asterisk on all their moves.
        if action_text.endswith('*'):
            action_text = action_text[:-1].strip()

        m = re.match(r'(\d+)[)] ([A-Za-z0-9_-]+): (.*)$', line)
        for rx, then in move_regexes.items():
            m = re.match(rx, action_text, re.IGNORECASE)
            if m is not None:
                cookedactions[-1].append(
                    then(attacker, attacker_name, *m.groups())
                )
                break
        else:
            for rx, then in move_regexes.items():
                m = re.match(rx[:-1] + ' (.*)', action_text, re.IGNORECASE)
                if m is not None:
                    cookedactions[-1].append(
                        then(attacker, attacker_name, *m.groups()[:-1])
                    )
                    break
            else:
                logging.error("Unrecognized move: %s" % action_text)
                for rx, then in move_regexes.items():
                    m = re.match(rx[:-1] + ' (.*)', action_text, re.IGNORECASE)
                    logging.error('%r = re.match(%r, %r, re.IGNORECASE)' % (m, rx[:-1] + ' (.*)', action_text))
                raise RuntimeError("surprise during parsing of the raw file")

    cookedlines = [wholemoveof(actions) for actions in cookedactions]
    while cookedlines and cookedlines[0] == 'pass':
        cookedlines = cookedlines[1:]
    return cookedlines


def verify_cooked_transcript(cookedlines):
    st = libannotate.newGame()
    attacker = 0
    for line in cookedlines:
        try:
            m = libannotate.WholeMove(line)
            st.apply(attacker, m)
            attacker = (1 - attacker)
        except ValueError as ex:
            return True, [str(ex), line]
    return False, ''


def attempt_common_fixups(cookedlines, keys, tried_repositioning_moves=False):
    failed, out = verify_cooked_transcript(cookedlines)
    if not failed:
        return False, cookedlines, keys

    # Sometimes a move like "sac y2; move r1 to Foo; catastrophe red at Foo; move b2 to Foo"
    # can be salvaged by just repositioning the mid-turn catastrophes to the end of the turn.
    # On the other hand, "sac y2; move r1 to Foo; catastrophe red at Foo; move r2 to Foo"
    # cannot be salvaged if the loss of that r2 would invalidate a later move.
    if out[0] == 'text did not parse as a move' and 'catastrophe' in out[1]:
        problematic_rx = r'catastrophe ([rygb])[a-z]+ at WHERE; .*move \1[123] from WHERE to \2'
        if re.search(expand_regex(problematic_rx), out[1], flags=re.IGNORECASE):
            return True, cookedlines, set(['midturn-catastrophe-then-move'])
        problematic_rx = r'catastrophe ([rygb])[a-z]+ at WHERE; .*build \1[123]'
        if re.search(expand_regex(problematic_rx), out[1], flags=re.IGNORECASE):
            return True, cookedlines, set(['midturn-catastrophe-then-build'])
        cookedlines2 = [(reposition_catastrophes(move) if move == out[1] else move) for move in cookedlines]
        if any(m1 != m2 for m1, m2 in zip(cookedlines, cookedlines2)):
            return attempt_common_fixups(cookedlines2, keys | set(['*repositioned-catastrophes']))

    # Leaving your own homeworld unoccupied is not allowed; but a lot of SDG games
    # end that way, in a "knocking over your king" kind of way. If we see such a
    # move, it should be the last move of the game, and removing it should yield
    # a valid transcript.
    if out[0] == 'The move as parsed was disallowed by the rule against self-destruction.' and out[1] == cookedlines[-1]:
        return attempt_common_fixups(cookedlines[:-1], keys | set(['*suicidal']))

    # Sometimes a move like "sac y2; move r1 from Home to Foo; move b1 from Foo to Home"
    # can be salvaged by just inverting the order of the moves. (Game 29784.)
    if not tried_repositioning_moves:
        if out[0] == 'The move as parsed was disallowed by the rule against self-destruction.' and re.match(r'.*; move.*; move.*', out[1]):
            cookedlines2 = [(reposition_moves(move) if move == out[1] else move) for move in cookedlines]
            return attempt_common_fixups(cookedlines2, keys | set(['*repositioned-moves']), tried_repositioning_moves=True)

    return True, cookedlines, set()


def convertRawHistoryToGameState(raw_history):
    rawlines = raw_history.splitlines()
    gamenumber, participants, rawlines = cook_header(rawlines)
    logging.error('1: %r %r %r\n', gamenumber, participants, rawlines)
    playercount = len(participants)
    assert playercount == 2
    cookedlines = cook_moves(gamenumber, participants, rawlines)
    logging.error('2: %r\n', cookedlines)
    failed, cookedlines, keys = attempt_common_fixups(cookedlines, set())
    logging.error('3: %r %r %r\n', failed, cookedlines, keys)
    assert not failed

    attacker = 0
    st = libannotate.newGame()
    for line in cookedlines:
        logging.error('4: applying move %s', line)
        m = libannotate.WholeMove(line)
        st.apply(attacker, m)
        attacker = (1 - attacker)
    logging.error('5: done')
    return st, attacker


def dealWithPost(post_body):
    fields = json.loads(post_body)
    logging.info('Got fields %r', fields)
    if looksLikeNewGame(fields):
        logging.info('Looks like a new game.')
        sdg = sdgbackend.SDG()
        for challenge in sdg.fetch_all_pending_challenges():
            sdg.accept_pending_challenge(challenge)
    elif looksLikePlayerMove(fields):
        logging.info('Looks like a player move.')
        game_id = extractGameNumberFromEmail(fields)
        sdg = sdgbackend.SDG()
        raw_history = sdg.fetch_history(game_id)
        st, attacker = convertRawHistoryToGameState(raw_history)
        move = st.getBestMove(attacker)
        sdg.submit_move(game_id, move.toSDGString())
    else:
        logging.error('Looks like nothing! Fields are %r', fields)
