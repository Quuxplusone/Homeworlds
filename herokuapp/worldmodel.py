import json
import logging
import libannotate
import os
import re
import requests

from bs4 import BeautifulSoup


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
    game_number_1 = m.group(1) if m else None
    m = re.match("It is now your turn to move in Homeworlds game #(\d+)[.].*", fields['TextBody'])
    game_number_2 = m.group(1) if m else None
    logging.info('Extracted game number %r from subject and %r from body', game_number_1, game_number_2)
    assert game_number_1 == game_number_2
    return int(game_number_1)


def goLogInAtSDG():
    session = requests.Session()
    r = session.post(
        'http://superdupergames.org/auth.html',
        data={
            'mode': 'auth',
            'username': os.environ['SUPERDUPERGAMES_USERNAME'],
            'password': os.environ['SUPERDUPERGAMES_PASSWORD'],
        },
    )
    if r.status_code == 200:
        logging.info('Successfully logged into SDG.')
    else:
        logging.error('Failed to log into SDG: response status code %d.' % r.status_code)
        logging.error(r.text)
        r.raise_for_status()
        logging.error('Continuing. Maybe the session is still okay.')
    return session


def goStartNewGamesAtSDG():
    session = goLogInAtSDG()
    r = session.get(
        'http://superdupergames.org'
    )
    rx = '<a href="(/[^"]*)">Accept Challenge</a>'
    for match in re.finditer(rx, r.text):
        url = match.group(1)
        # Click the link, thus accepting the challenge.
        logging.info('Attempting to start game %s' % url)
        r = session.get('http://superdupergames.org' + url)
        if r.status_code == 200:
            logging.info('Got status code 200 OK; game %s is presumed to be started now!' % url)
        else:
            logging.error('Got status code %d; game %s is presumed NOT to be started. Oops.' % r.status_code)
            logging.error(r.text)


def fetchSecretCodeFromSDG(session, game_number):
    r = session.get(
        'http://superdupergames.org/main.html',
        params={
            'page': 'play_homeworlds',
            'num': str(game_number),
        }
    )
    logging.warning(r.text)
    secret_code = None
    rx = '<input type="hidden" name="code" value="(.*)" />'
    for match in re.finditer(rx, r.text):
        if secret_code is not None:
            logging.error('Found multiple "hidden code" elements on page for game %r!', game_number)
            logging.error('First code: %r', secret_code)
            logging.error('Second code: %r', match.group(1))
        secret_code = match.group(1)
    return secret_code


def submitMoveToSDG(session, game_number, secret_code, text_of_move):
    r = session.post(
        'http://superdupergames.org/main.html',
        params={
            'page': 'play_homeworlds',
        },
        data={
            'mode': 'move',
            'num': str(game_number),
            'code': secret_code,
            'moves': text_of_move,
        },
    )
    if r.status_code == 200 and ('div class="quip"' not in r.text):
        logging.info('Got status code 200 OK; game %r is presumed to be moved-in now!', game_number)
        logging.info(r.text)
    else:
        logging.error('Got status code %r; game %r is presumed NOT to be moved-in. Oops.', r.status_code, game_number)
        logging.error(r.text)


def fetchRawGameHistoryFromSDG(session, game_number):
    r = session.get(
        'http://superdupergames.org/main.html',
        params={
            'page': 'archive_play',
            'gid': str(game_number),
        },
    )
    if r.status_code != 200:
        logging.error('Got status code %r from archive_play for game %r', r.status_code, game_number)
        logging.error(r.text)

    pagetext = r.text
    try:
        first = pagetext.index('Homeworlds Online (SDG#')
        last = pagetext.index('</p>', first)
        gametext_as_html = pagetext[first:last]
    except ValueError:
        if "Can't call method &quot;draw&quot; on an undefined value" in pagetext:
            print('    Game %d was invalid (never started)' % game_number)
        else:
            print('    Game %d was invalid for an unknown reason' % game_number)
            print(pagetext)
            raise Exception('invalid archived game text')

    soup = BeautifulSoup(gametext_as_html, features="html.parser")
    for br in soup.find_all("br"):
        br.replace_with("\n")
    return soup.get_text().strip()


def cookGameHistory(raw_history):
    # TODO FIXME BUG HACK
    return raw_history


def computeBestMoveFromHistory(raw_history):
    # TODO FIXME BUG HACK
    attacker = len(raw_history) % 2
    st = libannotate.stateFromSDGHistory(raw_history)
    text_of_move = st.getBestMove(attacker).toSDGString()
    return '\n'.join(text_of_move.split('; '))


def goMakeMoveAtSDG(game_number):
    session = goLogInAtSDG()
    logging.warning('OK, logged in at SDG')
    secret_code = fetchSecretCodeFromSDG(session, game_number)
    logging.warning('OK, got secret code %r', secret_code)
    raw_history = fetchRawGameHistoryFromSDG(session, game_number)
    logging.warning('OK, got raw history %r', raw_history)
    text_of_move = computeBestMoveFromHistory(raw_history)
    logging.warning('OK, got best move %r', text_of_move)
    submitMoveToSDG(session, game_number, secret_code, text_of_move)


def dealWithPost(post_body):
    fields = json.loads(post_body)
    logging.info('Got fields %r', fields)
    if looksLikeNewGame(fields):
        logging.info('Looks like a new game.')
        goStartNewGamesAtSDG()
    elif looksLikePlayerMove(fields):
        logging.info('Looks like a player move.')
        game_number = extractGameNumberFromEmail(fields)
        goMakeMoveAtSDG(game_number)
    else:
        logging.error('Looks like nothing! Fields are %r', fields)
