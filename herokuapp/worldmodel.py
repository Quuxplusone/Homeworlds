import json
import logging
import os
import re
import requests

from . import backend
from bs4 import BeautifulSoup


def createTables():
    backend.create_table_with_oids('posts', 'body TEXT')


def dropTables():
    with backend.cursor() as c:
        c.execute('DROP TABLE posts')


def receivePost(post_body, retry=True):
    try:
        with backend.cursor() as c:
            c.execute('INSERT INTO posts VALUES (?)', (post_body,))
    except Exception:
        logging.exception('failed to insert post_body')
        if retry:
            logging.exception('proceeding to create table...')
            createTables()
            receivePost(post_body, retry=False)


def getPosts(retry=True):
    try:
        result = []
        with backend.cursor() as c:
            for row in c.execute('SELECT body FROM posts'):
                result.append(row[0])
        return result
    except Exception:
        logging.exception('failed to insert post_body')
        if retry:
            logging.exception('proceeding to create table...')
            createTables()
            return getPosts(retry=False)


def looksLikeNewGame(fields):
    return (
        fields['From'] == 'admin@superdupergames.org' and
        fields['Subject'] == '[SDG] You have been challenged!'
    )


def looksLikePlayerMove(fields):
    return (
        fields['From'] == 'admin@superdupergames.org' and
        fields['Subject'].startswith('[SDG] Homeworlds game #') and
        fields['TextBody'].startswith('It is now your turn to move')
    )


def extractGameNumberFromEmail(fields):
    m = re.match("[SDG] Homeworlds game #(\d+) - It's your turn!", fields['Subject'])
    game_number_1 = m.group(1) if m else None
    m = re.match("It is now your turn to move in Homeworlds game #(\d+)[.].*", fields['TextBody'])
    game_number_2 = m.group(1) if m else None
    logging.warning('Extracted game number {} from subject and {} from body', game_number_1, game_number_2)
    assert game_number_1 == game_number_2
    return int(game_number_1)


def goLogInAtSDG():
    session = requests.Session()
    r = session.post(
        'http://superdupergames.org/auth.html',
        data={
            'username': os.environ['SUPERDUPERGAMES_USERNAME'],
            'password': os.environ['SUPERDUPERGAMES_PASSWORD'],
        },
    )
    if r.status_code == 200:
        logging.warning('Successfully logged into SDG.')
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
        logging.warning('Attempting to start game %s' % url)
        r = session.get('http://superdupergames.org' + url)
        if r.status_code == 200:
            logging.warning('Got status code 200 OK; game %s is presumed to be started now!' % url)
        else:
            logging.error('Got status code %d; game %s is presumed NOT to be started. Oops.' % r.status_code)
            logging.error(r.text)


def fetchSecretCodeFromSDG(session, game_number):
    r = session.get(
        'http://superdupergames.org/main.html',
        params = {
            'page': 'play_homeworlds',
            'num': str(game_number),
        }
    )
    secret_code = None
    rx = '<input type="hidden" name="code" value="(.*)" />'
    for match in re.finditer(rx, r.text):
        if secret_code is not None:
            logging.error('Found multiple "hidden code" elements on page for game {}!', game_number)
            logging.error('First code: {}', secret_code)
            logging.error('Second code: {}', match.group(1))
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
    if r.status_code == 200:
        logging.warning('Got status code 200 OK; game %s is presumed to be moved-in now!' % url)
    else:
        logging.error('Got status code %d; game %s is presumed NOT to be moved-in. Oops.' % r.status_code)
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
        logging.error('Got status code {} from archive_play for game {}', r.status_code, game_number)
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
    ## TODO FIXME BUG HACK
    return raw_history


def computeBestMoveFromHistory(cooked_history):
    ## TODO FIXME BUG HACK
    text_of_move = 'foo; bar; baz'
    return '\n'.join(text_of_move.split('; '))


def goMakeMovesAtSDG(game_number):
    session = goLogInAtSDG()
    logging.warning('OK, logged in at SDG')
    secret_code = fetchSecretCodeFromSDG(session, game_number)
    logging.warning('OK, got secret code {}', secret_code)
    raw_history = fetchRawGameHistoryFromSDG(session, game_number)
    logging.warning('OK, got raw history {}', raw_history)
    cooked_history = cookGameHistory(raw_history)
    text_of_move = computeBestMoveFromHistory(cooked_history)
    logging.warning('OK, got best move {}', text_of_move)
    submitMoveToSDG(session, game_number, secret_code, text_of_move)


def dealWithPost(post_body):
    fields = json.loads(post_body)
    logging.warning('Got post_body {}', post_body)
    logging.warning('Got fields {}', fields)
    if looksLikeNewGame(fields):
        logging.warning('Looks like a new game.')
        goStartNewGamesAtSDG()
    elif looksLikePlayerMove(fields):
        logging.warning('Looks like a player move.')
        goMakeMoveAtSDG(extractGameNumberFromEmail(fields))
    else:
        logging.warning('Looks like nothing.')
        pass
