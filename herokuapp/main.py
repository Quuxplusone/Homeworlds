#!/usr/bin/env python

import bottle
from bottle import Bottle
import libannotate
import logging
import os
import requests
import threading

from . import sdgbackend
from . import sqlbackend
from . import worldmodel

app = Bottle()
bottle.TEMPLATE_PATH.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), 'views'))


def do_asynchronously(task):
    t = threading.Thread(target=task, args=())
    t.daemon = True
    t.start()


def receivePost(post_body):
    try:
        sqlbackend.create_table_with_oids('posts', 'body TEXT')
    except:
        pass
    try:
        with sqlbackend.cursor() as c:
            c.execute('INSERT INTO posts VALUES (?)', (post_body,))
    except Exception:
        logging.exception('failed to INSERT post_body into the posts table')
        logging.error('post_body was: %r', post_body)
        raise


def getPosts():
    try:
        sqlbackend.create_table_with_oids('posts', 'body TEXT')
    except:
        pass
    try:
        result = []
        with sqlbackend.cursor() as c:
            for row in c.execute('SELECT body FROM posts'):
                result.append(row[0])
        return result
    except Exception:
        logging.exception('failed to SELECT from the posts table')
        raise


@app.get('/robots.txt')
def robots_txt():
    bottle.response.content_type = 'text/plain'
    return 'User-agent: *\nDisallow: /\n'


@app.post('/receive-mail')
def receive_mail():
    # http://docs.cloudmailin.com/receiving_email/http_status_codes/
    # https://stackoverflow.com/questions/14988887/reading-post-body-with-bottle-py
    try:
        post_body = bottle.request.body.read().decode('utf-8')
        logging.warning('Received POST: %s' % post_body)
        receivePost(post_body)
        worldmodel.dealWithPost(post_body)
        return bottle.HTTPResponse(status=200, body='Received')
    except Exception as e:
        logging.exception('Got some exception in /receive-mail')
        return bottle.HTTPResponse(status=500, body=str(e))


@app.get('/')
@app.get('/ai-move')
@app.get('/index.html')
def ai_move_get():
    sdg = sdgbackend.SDG()
    pending_challenges = sdg.fetch_all_pending_challenges()
    return bottle.template('ai-move-input.tpl', {
        'pending_challenges': pending_challenges,
    })

@app.get('/accept-challenge/<game_id>/<join_url:path>')
def accept_challenge_get(game_id, join_url):
    sdg = sdgbackend.SDG()
    challenge = {
        'game_id': game_id,
        'join_url': join_url,
    }
    do_asynchronously(
        lambda: sdg.accept_pending_challenge(challenge)
    )
    return bottle.redirect('/')

@app.get('/reject-challenge/<game_id>/<leave_url:path>')
def reject_challenge_get(game_id, leave_url):
    sdg = sdgbackend.SDG()
    challenge = {
        'game_id': game_id,
        'leave_url': leave_url,
    }
    do_asynchronously(
        lambda: sdg.reject_pending_challenge(challenge)
    )
    return bottle.redirect('/')

@app.post('/ai-move')
def ai_move_post():
    try:
        state_as_string = bottle.request.forms['state']
        attacker = int(bottle.request.forms['attacker'])
        st = libannotate.GameState(state_as_string)
        text_of_move = st.getBestMove(attacker).toSDGString()
        text_of_move = '\n'.join(text_of_move.split('; '))
        return bottle.template('ai-move-output.tpl', {
            'state_received': st.toString(),
            'attacker_received': attacker,
            'best_move': text_of_move,
        })
    except Exception as e:
        logging.exception('Got some exception in /ai-move')
        return bottle.template('ai-move-errorpage.tpl', {
            'state_received': bottle.request.forms.get('state'),
            'attacker_received': bottle.request.forms.get('attacker'),
            'error_text': str(e),
        })


@app.get('/get-history')
def get_history_get():
    return bottle.template('get-history-input.tpl', {})


@app.post('/get-history')
def get_history_post():
    try:
        game_id = int(bottle.request.forms['game_id'])
        sdg = sdgbackend.SDG()
        raw_history = sdg.fetch_history(game_id)
        return bottle.template('get-history-output.tpl', {
            'game_id_received': game_id,
            'raw_history': raw_history,
        })
    except Exception as e:
        return bottle.template('get-history-errorpage.tpl', {
            'game_id_received': bottle.request.forms.get('game_id'),
            'error_text': str(e),
        })


@app.get('/display-mail')
def display_mail():
    return bottle.template('display-mail.tpl', {
        'posts_received': getPosts(),
        'inbound_address': os.environ['POSTMARK_INBOUND_EMAIL_ADDRESS'],
    })


@app.get('/reset-database')
def reset_database():
    with sqlbackend.cursor() as c:
        c.execute('DROP TABLE posts')
    return bottle.HTTPResponse(status=200, body='OK, reset')


def helpfulPythonCommand():
    """
r = requests.post('http://speardane-homeworlds-bot.herokuapp.com/receive-mail', data={"Subject": "[SDG] Homeworlds game #35406 - It's your turn!", "TextBody": "It is now your turn to move in Homeworlds game #35406.  You have", "From": "admin@superdupergames.org"})
    """


def myReceiveHookUrl():
    return 'https://%s.herokuapp.com/receive-mail' % os.environ['HEROKU_APP_NAME']


def configure_postmark():
    r = requests.get(
        'https://api.postmarkapp.com/servers',
        headers={
            'Accept': 'application/json',
            'X-Postmark-Account-Token': os.environ['POSTMARK_ACCOUNT_API_TOKEN'],
        },
        params={
            'count': 100,
            'offset': 0,
        },
    )
    fields = r.json()
    for server in fields['Servers']:
        if server['InboundHookUrl'] == myReceiveHookUrl():
            os.environ['POSTMARK_INBOUND_EMAIL_ADDRESS'] = server['InboundAddress']
            return

    # Otherwise, we need to register our receive hook with Postmark.
    r = requests.post(
        'https://api.postmarkapp.com/servers',
        headers={
            'Accept': 'application/json',
            'X-Postmark-Account-Token': os.environ['POSTMARK_ACCOUNT_API_TOKEN'],
        },
        json={
            'Name': myReceiveHookUrl(),
            'InboundHookUrl': myReceiveHookUrl(),
        },
    )
    logging.warning('Received response from api.postmarkapp.com')
    logging.warning(r.text)
    fields = r.json()
    os.environ['POSTMARK_INBOUND_EMAIL_ADDRESS'] = fields['InboundAddress']


if __name__ == '__main__':
    sqlbackend.init()
    configure_postmark()
    app.run(host='0.0.0.0', port=os.environ['PORT'])
