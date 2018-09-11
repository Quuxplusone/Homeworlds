#!/usr/bin/env python

from bs4 import BeautifulSoup, NavigableString
import hashlib
import re
import requests
import sys

# This is a quick and dirty way of being able to publish this source code
# without revealing the cookie that would allow you to impersonate
# my (secondary) SDG account. My "encrypted_cookie" is protected by a
# human-readable password that I know but you don't.
#
# To generate an "encrypted_cookie" and password of your own, simply
# cut and paste this function into a Python interpreter, replace the
# current value of encrypted_cookie with your actual cookie, and then
# evaluate xor_cookie("your chosen password"). Take the result and
# paste it back into this script as the new value of "encrypted_cookie".
# Alternatively, you could replace this entire function with
#     return "your actual cookie"
# if you aren't going to distribute your patched source code.
#
# To find out the value of your own actual cookie in the first place,
# log into SDG and then inspect the value of your cookie storage.
# You should see a cookie named 'MasonX-Request-WithApacheSession-cookie'
# with a long hex string as its value.
#
def xor_cookie(password):
    m = hashlib.md5()
    m.update(password)
    encryption_key = m.hexdigest()
    encrypted_cookie = '6a55dfa88ad46e47ff076a4c4817997a'
    return '%x' % (int(encrypted_cookie, 16) ^ int(encryption_key, 16))

if __name__ == '__main__':
    password = sys.argv[1]

    cookies = {
        'MasonX-Request-WithApacheSession-cookie': xor_cookie(password),
    }

    r = requests.get(
        'http://superdupergames.org/main.html?page=listoldgames',
        cookies = cookies,
    )

    link_rx = r'"(/main.html\?page=archive_play&gid=(\d+))"'

    if re.search(link_rx, r.text) is None:
        print 'The page text did not look as expected. Perhaps your cookie has expired.'
        print 'The cookie we tried to use was:'
        for k, v in cookies.iteritems():
            print '    %s=%s' % (k, v)
        sys.exit(1)

    for m in re.finditer(link_rx, r.text):
        gamenumber = int(m.group(2))
        print '%d...' % gamenumber
        r2 = requests.get(
            'http://superdupergames.org' + m.group(1),
            cookies = cookies,
        )
        pagetext = r2.text
        try:
            first = pagetext.index('Homeworlds Online (SDG#')
            last = pagetext.index('</p>', first)
            gametext_as_html = pagetext[first:last]
        except ValueError:
            if "Can't call method &quot;draw&quot; on an undefined value" in pagetext:
                print '    Game %d was invalid (never started)' % gamenumber
            else:
                print '    Game %d was invalid for an unknown reason' % gamenumber
                print pagetext
            continue

        soup = BeautifulSoup(gametext_as_html, features="html.parser")
        for br in soup.find_all("br"):
            br.replace_with("\n")
        gametext = soup.get_text().strip().encode('utf-8')

        with open(str(gamenumber) + '.raw', 'w') as f:
            print >>f, gametext
