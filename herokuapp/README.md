
This bot uses CloudMailin for receiving email.
It expects to receive "email" via POST requests to
`https://speardane-homeworlds-bot.herokuapp.com/receive-mail`.

It stores each mail it receives in a PostgreSQL database.
You can see all the stored mail by visiting 
`https://speardane-homeworlds-bot.herokuapp.com/display-mail`.

===TODO===
For each received mail that looks like a SuperDuperGames move,
we parse it to extract the game number, and then we use speardane's
credentials to log into SuperDuperGames, read the progress of the game
so far, and produce our next move.

Then we submit the next move to SuperDuperGames by POSTing to the
appropriate form.



Deploying to Heroku
-------------------

Running `heroku create speardane-homeworlds-bot` should add
the following lines to your `.git/config`:

    [remote "heroku"]
        url = https://git.heroku.com/speardane-homeworlds-bot.git
        fetch = +refs/heads/*:refs/remotes/heroku/*

To build and deploy to localhost with Heroku:

    easy_install pip
    pip install -r requirements.txt
    brew install heroku
    heroku local
    open http://localhost:5000/

To build and deploy to the web with Heroku:

    brew install heroku
    heroku create speardane-homeworlds-bot
    git push heroku heroku:master
    heroku open

To configure SuperDuperGames and Postmark:

    heroku config:set 'SUPERDUPERGAMES_USERNAME=speardane'
    heroku config:set 'SUPERDUPERGAMES_PASSWORD=(password)'
    heroku config:set 'POSTMARK_ACCOUNT_API_TOKEN=(token)'

You can find the Account API Token on https://account.postmarkapp.com/api_tokens
The bot will use your API token to ask Postmark to set up an inbound email address
for itself. Once the app is running, you can retrieve that email address from 
`https://speardane-homeworlds-bot.herokuapp.com/display-mail`.
