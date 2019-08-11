import logging
import os
import re
import requests

from bs4 import BeautifulSoup


class SDG:

    def __init__(self):
        self.session_ = requests.Session()
        r = self.session_.post(
            'http://superdupergames.org/auth.html',
            data={
                'mode': 'auth',
                'username': os.environ['SUPERDUPERGAMES_USERNAME'],
                'password': os.environ['SUPERDUPERGAMES_PASSWORD'],
            },
        )
        r.raise_for_status()
        if r.status_code == 200:
            logging.info('Successfully logged into SDG.')
            return None
        logging.error('Failed to log into SDG: response status code %d.' % r.status_code)
        logging.error(r.text)
        raise RuntimeError('Failed to log into SDG: response status code %d.' % r.status_code)

    def fetch_all_pending_challenges(self):
        r = self.session_.get(
            'http://superdupergames.org'
        )
        r.raise_for_status()
        rx = r'<a href="([^"]*)">Accept Challenge</a><br />[[:space:]]*<a href="([^"]*)">Decline Challenge</a>'
        results = []
        for match in re.finditer(rx, r.text):
            join_url = match.group(1)
            leave_url = match.group(2)
            m = re.search('gameid=(\d+)$', join_url) or re.search('gameid=(\d+)$', leave_url)
            game_id = m.group(1) if m else None
            results.append({
                'id': game_id,
                'join_url': ('http://superdupergames.org' if join_url.startswith('/') else '') + join_url,
                'leave_url': ('http://superdupergames.org' if leave_url.startswith('/') else '') + leave_url,
            })
        return results

    def accept_pending_challenge(self, challenge):
        # Click the link, thus accepting the challenge.
        join_url = challenge['join_url']
        game_id = challenge['game_id']
        logging.info('Attempting to start game %s at %s' % (game_id, join_url))
        r = self.session_.get(join_url)
        r.raise_for_status()
        if r.status_code == 200:
            logging.info('Got status code 200 OK; game %s is presumed to be started now!' % game_id)
            return
        logging.error('Got status code %d while trying to start game %s. Oops.' % (game_id, r.status_code))
        logging.error(r.text)
        raise RuntimeError('Failed to start game: response status code %d.' % r.status_code)

    def fetch_hidden_code(self, game_id):
        r = self.session_.get(
            'http://superdupergames.org/main.html',
            params={
                'page': 'play_homeworlds',
                'num': str(game_id),
            }
        )
        r.raise_for_status()
        secret_code = None
        rx = '<input type="hidden" name="code" value="(.*)" />'
        for match in re.finditer(rx, r.text):
            if secret_code is not None:
                logging.error('Found multiple "hidden code" elements on page for game %r!', game_id)
                logging.error('First code: %r', secret_code)
                logging.error('Second code: %r', match.group(1))
            secret_code = match.group(1)
        if secret_code is None:
            raise RuntimeError('Failed to find any hidden code for game %r' % game_id)
        return secret_code

    def submit_move(self, game_id, text_of_move):
        text_of_move = '\n'.join(text_of_move.split('; '))
        hidden_code = self.fetch_hidden_code(game_id)
        r = self.session_.post(
            'http://superdupergames.org/main.html',
            params={
                'page': 'play_homeworlds',
            },
            data={
                'mode': 'move',
                'num': str(game_id),
                'code': hidden_code,
                'moves': text_of_move,
            },
        )
        if r.status_code == 200 and ('div class="quip"' in r.text):
            m = re.search('div class="quip"[^>]*>(.*)</div>', r.text)
            if m:
                sdg_quip = m.group(1)
                raise RuntimeError(sdg_quip)
            else:
                logging.error("SDG response to our move contained a quip but we couldn't parse it")
                logging.error(r.text)
                raise RuntimeError("SDG response to our move contained a quip but we couldn't parse it")
        elif r.status_code == 200:
            logging.info('Got status code 200 OK; game %r is presumed to be moved-in now!', game_id)
            logging.info(r.text)
        else:
            logging.error('Got status code %r; game %r is presumed NOT to be moved-in. Oops.', r.status_code, game_id)
            logging.error(r.text)

    def fetch_history(self, game_id):
        r = self.session_.get(
            'http://superdupergames.org/main.html',
            params={
                'page': 'archive_play',
                'gid': str(game_id),
            },
        )
        r.raise_for_status()
        if r.status_code != 200:
            logging.error('Got status code %r from archive_play for game %r', r.status_code, game_id)
            logging.error(r.text)

        pagetext = r.text
        try:
            first = pagetext.index('Homeworlds Online (SDG#')
            last = pagetext.index('</p>', first)
            gametext_as_html = pagetext[first:last]
        except ValueError:
            if "Can't call method &quot;draw&quot; on an undefined value" in pagetext:
                raise RuntimeError('Game %d was invalid (never started)' % game_id)
            else:
                logging.error(pagetext)
                raise RuntimeError('Game %d was invalid for an unknown reason' % game_id)

        soup = BeautifulSoup(gametext_as_html, features="html.parser")
        for br in soup.find_all("br"):
            br.replace_with("\n")
        return soup.get_text().strip()
