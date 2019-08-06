#!/usr/bin/env python

import bottle
from bottle import Bottle
import logging
import os
import requests

from . import backend
from . import worldmodel

app = Bottle()
bottle.TEMPLATE_PATH.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), 'views'))


@app.get('/robots.txt')
def robots_txt():
    bottle.response.content_type = 'text/plain'
    return 'User-agent: *\nDisallow: /\n'


@app.get('/')
@app.get('/index.html')
def home():
    return bottle.static_file('index.html', root='./herokuapp/static', mimetype='text/html')


@app.post('/receive-mail')
def receive_mail():
    # http://docs.cloudmailin.com/receiving_email/http_status_codes/
    # https://stackoverflow.com/questions/14988887/reading-post-body-with-bottle-py
    try:
        post_body = bottle.request.body.read().decode('utf-8')
        logging.warning('Received POST: %s' % post_body)
        worldmodel.receivePost(post_body)
        worldmodel.dealWithPost(post_body)
        return bottle.HTTPResponse(status=200, body='Received')
    except:
        return bottle.HTTPResponse(status=500, body='Some exception was thrown, I dunno')


@app.get('/display-mail')
def display_mail():
    return bottle.template('display-mail.tpl', {
        'posts_received': worldmodel.getPosts(),
        'inbound_address': os.environ['POSTMARK_INBOUND_EMAIL_ADDRESS'],
    })


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
    backend.init()
    configure_postmark()
    app.run(host='0.0.0.0', port=os.environ['PORT'])
