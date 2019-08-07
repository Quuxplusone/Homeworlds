#!/usr/bin/env python

import bottle
from bottle import Bottle
import logging
import os
import requests

from . import sqlbackend
from . import worldmodel

app = Bottle()
bottle.TEMPLATE_PATH.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), 'views'))


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
@app.get('/index.html')
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
