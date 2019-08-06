import json
import logging
import os
import re
import requests

from . import backend


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


def goMakeMovesAtSDG():
    session = goLogInAtSDG()
    # TODO FIXME BUG HACK
    pass


def dealWithPost(post_body):
    fields = json.loads(post_body)
    logging.warning('Got post_body {}', post_body)
    logging.warning('Got fields {}', fields)
    if looksLikeNewGame(fields):
        logging.warning('Looks like a new game.')
        goStartNewGamesAtSDG()
    elif looksLikePlayerMove(fields):
        logging.warning('Looks like a player move.')
        goMakeMovesAtSDG()
    else:
        logging.warning('Looks like nothing.')
        pass
