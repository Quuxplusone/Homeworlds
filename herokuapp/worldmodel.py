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


def convertRawHistoryToGameState(raw_history):
    # TODO FIXME BUG HACK
    attacker = len(raw_history) % 2
    st = libannotate.GameState(raw_history)
    return st, attacker


def dealWithPost(post_body):
    fields = json.loads(post_body)
    logging.info('Got fields %r', fields)
    if looksLikeNewGame(fields):
        logging.info('Looks like a new game.')
        sdg = sdgbackend.SDG()
        for challenge in sdg.fetch_pending_challenges():
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
